package instructions

import AsmLabeling._


sealed abstract class PredefinedFunc(val name: String) {
  def toAsmFunc(): AsmFunction

  override def toString: String = s"_$name"
}

sealed abstract class PredefinedErrMsg(override val name: String, val errMsg: String) extends PredefinedFunc(name) {
  override def toAsmFunc(): AsmFunction = AsmFunction(
    LabelledStringConst(
      asmLocal ~ s"._${name}_str0",
      s"fatal error: $errMsg\n"
    ),
    LabelHeader(name),
    ADR(XRegister(0), asmLocal ~ s"._${name}_str0"),
    BL(asmGlobal ~ "_prints"),
    MOV(WRegister(0), ImmVal(-1)),
    BL(asmGlobal ~ "exit")
  )
}

sealed abstract class PredefinedRead(override val name: String, val fmt: String) extends PredefinedFunc(name) {
  override def toAsmFunc(): AsmFunction = AsmFunction(
    LabelledStringConst(asmLocal ~ s"._${name}_str0", fmt),
    LabelHeader(asmGlobal ~ s"_$name"),
    Comment("X0 contains the \"original\" value of the destination of the read")(4),
    Comment("allocate space on the stack to store the read: preserve alignment!")(4),
    Comment("the passed default argument should be stored in case of EOF")(4),
    Comment(
      "aarch64 mandates 16-byte SP alignment at all times, " +
        "might as well merge the stores"
    )(4),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    MOV(XRegister(1), sp),
    ADR(XRegister(0), asmLocal ~ s"._${name}_str0"),
    BL(asmGlobal ~ "scanf"),
    LDP(XRegister(0), lr, PostIndex(sp, ImmVal(16))),
    RET
  )
}

sealed abstract class PredefinedPrint(
  override val name: String, val fmt: String, val setupRegisters: Seq[Instruction] = Nil
) extends PredefinedFunc(name) {
  override def toAsmFunc(): AsmFunction = {
    val fmtStr = asmLocal ~ s"._${name}_str0"
    AsmFunction(
      LabelledStringConst(fmtStr, fmt),
      LabelHeader(asmGlobal ~ s"_$name"),
      STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
      AsmFunction(setupRegisters*),
      ADR(XRegister(0), fmtStr),
      BL(asmGlobal ~ (if (name == "println") "puts" else "printf")),
      MOV(XRegister(0), ImmVal(0)),
      BL(asmGlobal ~ "fflush"),
      LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
      RET
    )
  }
}

sealed abstract class PredefinedMemory(override val name: String) extends PredefinedFunc(name)

case object PErrDivZero extends PredefinedErrMsg("errDivZero", "division or modulo by zero")
case object PErrOutOfMemory extends PredefinedErrMsg("errOutOfMemory", "out of memory")
case object PErrOverflow extends PredefinedErrMsg("errOverflow", "integer overflow or underflow occurred")
case object PErrNull extends PredefinedErrMsg("errNull", "null pair dereferenced or freed")
case object PErrOutOfBounds extends PredefinedErrMsg("errOutOfBounds", "array index %d out of bounds")
case object PErrBadChar extends PredefinedErrMsg("errBadChar", "pint %d is not ascii character 0-127")

case object PReadc extends PredefinedRead("readc", "%c")
case object PReadi extends PredefinedRead("readi", "%d")

case object PPrintp extends PredefinedPrint("printp", "%p", Seq(MOV(XRegister(1), XRegister(0))))
case object PPrintln extends PredefinedPrint("println", "")
case object PPrinti extends PredefinedPrint("printi", "%d", Seq(MOV(XRegister(1), XRegister(0))))
case object PPrintc extends PredefinedPrint("printc", "%c", Seq(MOV(XRegister(1), XRegister(0))))
case object PPrints extends PredefinedPrint(
  "prints",
  "%.*s",
  Seq(
    MOV(XRegister(2), XRegister(0)),
    LDUR(WRegister(1), Offset(XRegister(0), ImmVal(-4)))
  )
)
case object PPrintb extends PredefinedPrint("printb", "") {
  override def toAsmFunc(): AsmFunction = {
    val falseStr = asmLocal ~ "._printb_str0"
    val trueStr = asmLocal ~ "._printb_str1"
    val fmtStr = asmLocal ~ "._printb_str2"

    AsmFunction(
      LabelledStringConst(falseStr, "false"),
      LabelledStringConst(trueStr, "true"),
      LabelledStringConst(fmtStr, "%.*s"),
      LabelHeader(asmGlobal ~ "_printb"),
      STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
      CMP(WRegister(0), ImmVal(0)),
      BCond(asmLocal ~ "_printb0", Cond.NE),
      ADR(XRegister(2), falseStr),
      B(asmLocal ~ "_printb1"),
      LabelHeader(asmLocal ~ "_printb0"),
      ADR(XRegister(2), trueStr),
      LabelHeader(asmLocal ~ "_printb1"),
      LDUR(WRegister(1), Offset(XRegister(2), ImmVal(-4))),
      ADR(XRegister(0), fmtStr),
      BL(asmGlobal ~ "printf"),
      MOV(XRegister(0), ImmVal(0)),
      BL(asmGlobal ~ "fflush"),
      LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
      RET
    )
  }
}

case object PFreepair extends PredefinedMemory("freepair") {
  override def toAsmFunc(): AsmFunction = AsmFunction(
    LabelHeader(asmGlobal ~ "_freepair"),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    CBZ(XRegister(0), asmGlobal ~ "_errNull"),
    BL(asmGlobal ~ "free"),
    LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
    RET
  )
}
case object PMalloc extends PredefinedMemory("malloc") {
  override def toAsmFunc(): AsmFunction = AsmFunction(
    LabelHeader(asmGlobal ~ "_malloc"),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    BL(asmGlobal ~ "malloc"),
    CBZ(XRegister(0), asmGlobal ~ "_errOutOfMemory"),
    LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
    RET
  )
}

object PredefinedFunctions {
  private val funcList: List[PredefinedFunc] = List(
    PErrOutOfBounds,
    PErrNull,
    PErrOverflow,
    PErrDivZero,
    PErrOutOfMemory,
    PErrBadChar,
    PReadc,
    PReadi,
    PFreepair,
    PMalloc,
    PPrintp,
    PPrintln,
    PPrinti,
    PPrintc,
    PPrints,
    PPrintb
  )

  private val asmFunclist: List[AsmFunction] = funcList.map(_.toAsmFunc())

  val predefinedFunctions: Map[String, AsmFunction] = funcList.map(_.toString).zip(asmFunclist).toMap
}
