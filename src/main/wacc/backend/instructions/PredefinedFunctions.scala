package backend.instructions

import AsmLabeling.*

private final val x0 = XRegister(0)
private final val x1 = XRegister(1)
private final val x2 = XRegister(2)
private final val w0 = WRegister(0)
private final val w1 = WRegister(1)

sealed abstract class PredefinedFunc(val name: String):
  val toAsmFunc: AsmFunction

  override def toString: String = f"_$name"

sealed abstract class PredefinedErrMsg(override val name: String, val errMsg: String)
    extends PredefinedFunc(name):
  override val toAsmFunc: AsmFunction = AsmFunction(
    LabelledStringConst(
      asmLocal ~ f"._${name}_str0",
      f"fatal error: $errMsg\n"
    ),
    LabelHeader(name),
    ADR(x0, asmLocal ~ s"._${name}_str0"),
    BL(asmGlobal ~ "_prints"),
    MOV(w0, ImmVal(-1)),
    BL(asmGlobal ~ "exit")
  )

sealed abstract class PredefinedRead(override val name: String, val fmt: String)
    extends PredefinedFunc(name):
  override val toAsmFunc: AsmFunction = AsmFunction(
    LabelledStringConst(asmLocal ~ f"._${name}_str0", fmt),
    LabelHeader(asmGlobal ~ f"_$name"),
    Comment("X0 contains the \"original\" value of the destination of the read")(4),
    Comment("allocate space on the stack to store the read: preserve alignment!")(4),
    Comment("the passed default argument should be stored in case of EOF")(4),
    Comment(
      "aarch64 mandates 16-byte SP alignment at all times, " +
        "might as well merge the stores"
    )(4),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    MOV(x1, sp),
    ADR(x0, asmLocal ~ s"._${name}_str0"),
    BL(asmGlobal ~ "scanf"),
    LDP(x0, lr, PostIndex(sp, ImmVal(16))),
    RET
  )

sealed abstract class PredefinedPrint(
    override val name: String,
    val fmt: String,
    val setupRegisters: Seq[Instruction] = Nil
) extends PredefinedFunc(name):
  override val toAsmFunc: AsmFunction =
    val fmtStr = asmLocal ~ f"._${name}_str0"
    AsmFunction(
      LabelledStringConst(fmtStr, fmt),
      LabelHeader(asmGlobal ~ f"_$name"),
      STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
      AsmFunction(setupRegisters*),
      ADR(x0, fmtStr),
      BL(asmGlobal ~ (if (name == "println") "puts" else "printf")),
      MOV(x0, ImmVal(0)),
      BL(asmGlobal ~ "fflush"),
      LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
      RET
    )

sealed abstract class PredefinedArr(
    override val name: String
) extends PredefinedFunc(name):
  override val toAsmFunc: AsmFunction =
    val setupRegister: AsmSnippet = name match {
      case "arrLoad1" => LDRB(WRegister(7), RegisterAddress(XRegister(7), XRegister(17)))
      case "arrLoad4" =>
        LDR(WRegister(7), RegisterAddress(XRegister(7), XRegister(17), Some(LSL(ImmVal(2)))))
      case "arrLoad8" =>
        LDR(XRegister(7), RegisterAddress(XRegister(7), XRegister(17), Some(LSL(ImmVal(3)))))
      case "arrStore1" => STRB(WRegister(8), RegisterAddress(XRegister(7), XRegister(17)))
      case "arrStore4" =>
        STR(WRegister(8), RegisterAddress(XRegister(7), XRegister(17), Some(LSL(ImmVal(2)))))
      case "arrStore8" =>
        STR(XRegister(8), RegisterAddress(XRegister(7), XRegister(17), Some(LSL(ImmVal(3)))))
      case _ => EmptyAsmSnippet
    }
    AsmFunction(
      LabelHeader(asmGlobal ~ f"_$name"),
      STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
      CMP(WRegister(17), ImmVal(0)),
      CSEL(XRegister(1), XRegister(17), XRegister(1), Cond.LT),
      BCond("_errOutOfBounds", Cond.LT),
      LDUR(WRegister(30), Offset(XRegister(7), ImmVal(-4))),
      CMP(WRegister(17), WRegister(30)),
      CSEL(XRegister(1), XRegister(17), XRegister(1), Cond.GE),
      BCond("_errOutOfBounds", Cond.GE),
      setupRegister,
      LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
      RET
    )

sealed abstract class PredefinedMemory(override val name: String) extends PredefinedFunc(name)

case object P_ErrDivZero extends PredefinedErrMsg("errDivZero", "division or modulo by zero")
case object P_ErrOutOfMemory extends PredefinedErrMsg("errOutOfMemory", "out of memory")
case object P_ErrOverflow
    extends PredefinedErrMsg("errOverflow", "integer overflow or underflow occurred")
case object P_ErrNull extends PredefinedErrMsg("errNull", "null pair dereferenced or freed")
case object P_ErrOutOfBounds
    extends PredefinedErrMsg("errOutOfBounds", "array index %d out of bounds")
case object P_ErrBadChar
    extends PredefinedErrMsg("errBadChar", "pint %d is not ascii character 0-127")

case object P_Readc extends PredefinedRead("readc", "%c")
case object P_Readi extends PredefinedRead("readi", "%d")

case object P_Printp extends PredefinedPrint("printp", "%p", Seq(MOV(x1, x0)))
case object P_Println extends PredefinedPrint("println", "")
case object P_Printi extends PredefinedPrint("printi", "%d", Seq(MOV(x1, x0)))
case object P_Printc extends PredefinedPrint("printc", "%c", Seq(MOV(x1, x0)))
case object P_Prints extends PredefinedPrint(
  "prints",
  "%.*s",
  Seq(
    MOV(x2, x0),
    LDUR(w1, Offset(x0, ImmVal(-4)))
  )
)
case object P_Printb extends PredefinedPrint("printb", "") {
  override val toAsmFunc: AsmFunction = {
    val falseStr = asmLocal ~ "._printb_str0"
    val trueStr = asmLocal ~ "._printb_str1"
    val fmtStr = asmLocal ~ "._printb_str2"

    AsmFunction(
      LabelledStringConst(falseStr, "false"),
      LabelledStringConst(trueStr, "true"),
      LabelledStringConst(fmtStr, "%.*f"),
      LabelHeader(asmGlobal ~ "_printb"),
      STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
      CMP(w0, ImmVal(0)),
      BCond(asmLocal ~ "_printb0", Cond.NE),
      ADR(x2, falseStr),
      B(asmLocal ~ "_printb1"),
      LabelHeader(asmLocal ~ "_printb0"),
      ADR(x2, trueStr),
      LabelHeader(asmLocal ~ "_printb1"),
      LDUR(w1, Offset(x2, ImmVal(-4))),
      ADR(x0, fmtStr),
      BL(asmGlobal ~ "printf"),
      MOV(x0, ImmVal(0)),
      BL(asmGlobal ~ "fflush"),
      LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
      RET
    )
  }
}
case object P_Freepair extends PredefinedMemory("freepair") {
  override val toAsmFunc: AsmFunction = AsmFunction(
    LabelHeader(asmGlobal ~ "_freepair"),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    CBZ(x0, asmGlobal ~ "_errNull"),
    BL(asmGlobal ~ "free"),
    LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
    RET
  )
}
case object P_Malloc extends PredefinedMemory("malloc") {
  override val toAsmFunc: AsmFunction = AsmFunction(
    LabelHeader(asmGlobal ~ "_malloc"),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    BL(asmGlobal ~ "malloc"),
    CBZ(x0, asmGlobal ~ "_errOutOfMemory"),
    LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
    RET
  )
}
case object P_ArrLoad1 extends PredefinedArr("arrLoad1")

case object P_ArrLoad4 extends PredefinedArr("arrLoad4")

case object P_ArrLoad8 extends PredefinedArr("arrLoad8")

case object P_ArrStore1 extends PredefinedArr("arrStore1")

case object P_ArrStore4 extends PredefinedArr("arrStore4")

case object P_ArrStore8 extends PredefinedArr("arrStore8")


object PredefinedFunctions {
  private val funcList: List[PredefinedFunc] = List(
    P_ErrOutOfBounds,
    P_ErrNull,
    P_ErrOverflow,
    P_ErrDivZero,
    P_ErrOutOfMemory,
    P_ErrBadChar,
    P_Readc,
    P_Readi,
    P_Freepair,
    P_Malloc,
    P_Printp,
    P_Println,
    P_Printi,
    P_Printc,
    P_Prints,
    P_Printb,
    P_ArrLoad1,
    P_ArrLoad4,
    P_ArrLoad8, 
    P_ArrStore1, 
    P_ArrStore4, 
    P_ArrStore8
  )

  private val asmFunclist: List[AsmFunction] = funcList.map(_.toAsmFunc)

  val predefinedFunctions: Map[PredefinedFunc, AsmFunction] = funcList.zip(asmFunclist).toMap
}

