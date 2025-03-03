package instructions

import Register.*
import AsmLabeling.*

// TODO: .align 4, .word n

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
    LabelledStringConst(asmLocal ~ localErrName, predefinedErrMessages(localErrName)),
    InstrLabel(asmLocal ~ localErrName),
    ADR(XRegister(0), asmLocal ~ localErrName),
    BL(asmGlobal ~ "_prints"),
    MOV(WRegister(0), ImmVal(-1)),
    BL(asmGlobal ~ "exit")
  )

  def _errOutOfBounds() = _err("._errDivZero_str0")

  def _errNull() = _err("._errNull_str0")

  def _errOverflow() = _err("._errOverflow_str0")

  def _errDivZero() = _err("._errDivZero_str0")

  def _errOutOfMemory() = _err("._errOutOfMemory_str0")

  def _readc() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._readc_str0", "%c"),
    InstrLabel("_readc"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    MOV(XRegister(1), SP),
    ADR(XRegister(0), asmLocal ~ "._readc_str0"),
    BL(asmGlobal ~ "scanf"),
    LDP(XRegister(0), LR, PostIndex(SP, ImmVal(16))),
    RET
  )

  def _readi() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._readi_str0", "%d"),
    InstrLabel("_readi"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    MOV(XRegister(1), SP),
    ADR(XRegister(0), asmLocal ~ "_readi_str0"),
    BL(asmGlobal ~ "scanf"),
    LDP(XRegister(0), LR, PostIndex(SP, ImmVal(16))),
    RET
  )

  def _freepair() = AsmFunction(
    InstrLabel("_freepair"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    CBZ(XRegister(0), asmGlobal ~ "_errNull"),
    BL(asmGlobal ~ "free"),
    LDP(LR, XZR, PostIndex(SP, ImmVal(16))),
    RET
  )

  def _malloc() = AsmFunction(
    InstrLabel("_malloc"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    BL(asmGlobal ~ "malloc"),
    CBZ(XRegister(0), asmGlobal ~ "_errOutOfMemory"),
    LDP(LR, XZR, PostIndex(SP, ImmVal(16))),
    RET
  )

  def _printp() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._printp_str0", "%p"),
    InstrLabel("_printp"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    MOV(XRegister(1), XRegister(0)),
    ADR(XRegister(0), asmLocal ~ "._printp_str0"),
    BL(asmGlobal ~ "printf"),
    MOV(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, PostIndex(SP, ImmVal(16))),
    RET
  )

  def _println() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._println_str0", ""),
    InstrLabel("_println"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    ADR(XRegister(0), asmLocal ~ "._println_str0"),
    BL(asmGlobal ~ "puts"),
    MOV(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, PostIndex(SP, ImmVal(16))),
    RET
  )

  def _printi() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._printi_str0", "%d"),
    InstrLabel("_printi"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    MOV(XRegister(1), XRegister(0)),
    ADR(XRegister(0), asmLocal ~ "._printi_str0"),
    BL(asmGlobal ~ "printf"),
    MOV(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, PostIndex(SP, ImmVal(16))),
    RET
  )

  def _printc() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._printc_str0", "%c"),
    InstrLabel("_printc"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    MOV(XRegister(1), XRegister(0)),
    ADR(XRegister(0), asmLocal ~ "._printc_str0"),
    BL(asmGlobal ~ "printf"),
    MOV(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, PostIndex(SP, ImmVal(16))),
    RET
  )

  def _prints() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._prints_str0", "%s"),
    InstrLabel("_prints"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    MOV(XRegister(2), XRegister(0)),
    LDUR(WRegister(1), Offset(XRegister(0), ImmVal(-4))),
    ADR(XRegister(0), asmLocal ~ "._prints_str0"),
    BL(asmGlobal ~ "printf"),
    MOV(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, PostIndex(SP, ImmVal(16))),
    RET
  )

  def _printb() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._printb_str0", "false"),
    InstrLabel("_printb"),
    STP(LR, XZR, PreIndex(SP, ImmVal(-16))),
    CMPImm(WRegister(0), ImmVal(0)),
    BCond(asmGlobal ~ "L_printb0", Cond.NE),
    ADR(XRegister(2), asmLocal ~ "._preintb_str0"),
    B(asmGlobal ~ ".L_printb1")
  )

  def L_printb0() = AsmFunction(
    LabelledStringConst(asmLocal ~ "._printb_str1", "true"),
    InstrLabel(".L_printb0"),
    ADR(XRegister(2), asmLocal ~ "._printb_str1")
  )

  def L_printb1() = AsmFunction(
    LabelledStringConst(asmLocal ~ ".printb_str2", "%.*s"),
    InstrLabel(".L_printb1"),
    LDUR(WRegister(1), Offset(XRegister(2), ImmVal(-4))),
    ADR(XRegister(0), asmLocal ~ ".printb_str2"),
    BL(asmGlobal ~ "printf"),
    MOV(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, PostIndex(SP, ImmVal(16))),
    RET
  )
}
