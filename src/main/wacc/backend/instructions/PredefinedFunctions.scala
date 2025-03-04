package instructions

import AsmLabeling.*

object PredefinedFunctions {
  val predefinedErrMessages = Map(
    "._errDivZero_str0" ->
      "fatal error: division or modulo by zero\n",
    "._errOutOfMemory_str0" ->
      "fatal error: out of memory\n",
    "._errOverflow_str0" ->
      "fatal error: integer overflow or underflow occurred\n",
    "._errNull_str0" ->
      "fatal error: null pair dereferenced or freed\n",
    "._errOutOfBounds_str0" ->
      "fatal error: array index %d out of bounds\n"
  )

  def getPredefinedMessages: String =
    predefinedErrMessages.view.mapValues(_.toString).mkString("\n")

  def _err(localErrName: String) = AsmFunction(
    LabelledStringConst(
      asmLocal ~ f"${localErrName}_str0",
      predefinedErrMessages(f"${localErrName}_str0")
    ),
    LabelHeader(localErrName),
    ADR(XRegister(0), asmLocal ~ f"${localErrName}_str0"),
    BL(asmGlobal ~ "_prints"),
    MOV(WRegister(0), ImmVal(-1)),
    BL(asmGlobal ~ "exit")
  )

  def _errOutOfBounds() = _err("._errOutOfBounds")

  def _errNull() = _err("._errNull")

  def _errOverflow() = _err("._errOverflow")

  def _errDivZero() = _err("._errDivZero")

  def _errOutOfMemory() = _err("._errOutOfMemory")

  def _readc() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._readc_str0", " %c"),
    LabelHeader(asmGlobal ~ "_readc"),
    Comment("X0 contains the \"original\" value of the destination of the read")(4),
    Comment("allocate space on the stack to store the read: preserve alignment!")(4),
    Comment("the passed default argument should be stored in case of EOF")(4),
    Comment(
      "aarch64 mandates 16-byte SP alignment at all times, " +
        "might as well merge the stores"
    )(4),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    MOV(XRegister(1), sp),
    ADR(XRegister(0), asmLocal ~ "._readc_str0"),
    BL(asmGlobal ~ "scanf"),
    LDP(XRegister(0), lr, PostIndex(sp, ImmVal(16))),
    RET
  )

  def _readi() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._readi_str0", "%d"),
    LabelHeader(asmGlobal ~ "_readi"),
    Comment("X0 contains the \"original\" value of the destination of the read")(4),
    Comment("allocate space on the stack to store the read: preserve alignment!")(4),
    Comment("the passed default argument should be stored in case of EOF")(4),
    Comment(
      "aarch64 mandates 16-byte SP alignment at all times," +
        " might as well merge the stores"
    )(4),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    MOV(XRegister(1), sp),
    ADR(XRegister(0), asmLocal ~ "_readi_str0"),
    BL(asmGlobal ~ "scanf"),
    LDP(XRegister(0), lr, PostIndex(sp, ImmVal(16))),
    RET
  )

  def _freepair() = AsmFunction(
    LabelHeader(asmGlobal ~ "_freepair"),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    CBZ(XRegister(0), asmGlobal ~ "_errNull"),
    BL(asmGlobal ~ "free"),
    LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
    RET
  )

  def _malloc() = AsmFunction(
    LabelHeader(asmGlobal ~ "_malloc"),
    STP(lr, xzr, PreIndex(sp, ImmVal(-16))),
    BL(asmGlobal ~ "malloc"),
    CBZ(XRegister(0), asmGlobal ~ "_errOutOfMemory"),
    LDP(lr, xzr, PostIndex(sp, ImmVal(16))),
    RET
  )

  def _print(name: String, fmt: String, setupRegisters: Seq[Instruction] = Nil) = {
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

  def _printp() = _print("printp", "%p", Seq(MOV(XRegister(1), XRegister(0))))

  def _println() = _print("println", "")

  def _printi() = _print("printi", "%d", Seq(MOV(XRegister(1), XRegister(0))))

  def _printc() = _print("printc", "%c", Seq(MOV(XRegister(1), XRegister(0))))

  def _prints() = _print(
    "prints",
    "%.*s",
    Seq(
      MOV(XRegister(2), XRegister(0)),
      LDUR(WRegister(1), Offset(XRegister(0), ImmVal(-4)))
    )
  )

  def _printb() = {
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
