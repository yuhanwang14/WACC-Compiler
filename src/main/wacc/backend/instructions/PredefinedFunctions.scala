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

  def _err(localErrName: String) = List(
    LabelledStringConst(asmLocal ~ localErrName, predefinedErrMessages(localErrName)),
    InstrLabel(asmLocal ~ localErrName),
    ADR(XRegister(0), asmLocal ~ localErrName),
    BL(asmGlobal ~ "_prints"),
    MOVImm(WRegister(0), ImmVal(-1)),
    BL(asmGlobal ~ "exit")
  )

  def _errOutOfBounds() = _err("._errDivZero_str0")

  def _errNull() = _err("._errNull_str0")

  def _errOverflow() = _err("._errOverflow_str0")

  def _errDivZero() = _err("._errDivZero_str0")

  def _errOutOfMemory() = _err("._errOutOfMemory_str0")

  def _readc() = List(
    LabelledStringConst(asmLocal ~ "._readc_str0", "%c"),
    InstrLabel("_readc"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    MOVReg(XRegister(1), SP),
    ADR(XRegister(0), asmLocal ~ "._readc_str0"),
    BL(asmGlobal ~ "scanf"),
    LDP(XRegister(0), LR, SP, ImmVal(16), PostIndex),
    RET
  )

  def _readi() = List(
    LabelledStringConst(asmLocal ~ "._readi_str0", "%d"),
    InstrLabel("_readi"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    MOVReg(XRegister(1), SP),
    ADR(XRegister(0), asmLocal ~ "_readi_str0"),
    BL(asmGlobal ~ "scanf"),
    LDP(XRegister(0), LR, SP, ImmVal(16), PostIndex),
    RET
  )

  def _freepair() = List(
    InstrLabel("_freepair"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    CBZ(XRegister(0), asmGlobal ~ "_errNull"),
    BL(asmGlobal ~ "free"),
    LDP(LR, XZR, SP, ImmVal(16), PostIndex),
    RET
  )

  def _malloc() = List(
    InstrLabel("_malloc"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    BL(asmGlobal ~ "malloc"),
    CBZ(XRegister(0), asmGlobal ~ "_errOutOfMemory"),
    LDP(LR, XZR, SP, ImmVal(16), PostIndex),
    RET
  )

  def _printp() = List(
    LabelledStringConst(asmLocal ~ "._printp_str0", "%p"),
    InstrLabel("_printp"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    MOVReg(XRegister(1), XRegister(0)),
    ADR(XRegister(0), asmLocal ~ "._printp_str0"),
    BL(asmGlobal ~ "printf"),
    MOVImm(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, SP, ImmVal(16), PostIndex),
    RET
  )

  def _println() = List(
    LabelledStringConst(asmLocal ~ "._println_str0", ""),
    InstrLabel("_println"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    ADR(XRegister(0), asmLocal ~ "._println_str0"),
    BL(asmGlobal ~ "puts"),
    MOVImm(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, SP, ImmVal(16), PostIndex),
    RET
  )

  def _printi() = List(
    LabelledStringConst(asmLocal ~ "._printi_str0", "%d"),
    InstrLabel("_printi"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    MOVReg(XRegister(1), XRegister(0)),
    ADR(XRegister(0), asmLocal ~ "._printi_str0"),
    BL(asmGlobal ~ "printf"),
    MOVImm(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, SP, ImmVal(16), PostIndex),
    RET
  )

  def _printc() = List(
    LabelledStringConst(asmLocal ~ "._printc_str0", "%c"),
    InstrLabel("_printc"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    MOVReg(XRegister(1), XRegister(0)),
    ADR(XRegister(0), asmLocal ~ "._printc_str0"),
    BL(asmGlobal ~ "printf"),
    MOVImm(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, SP, ImmVal(16), PostIndex),
    RET
  )

  def _prints() = List(
    LabelledStringConst(asmLocal ~ "._prints_str0", "%s"),
    InstrLabel("_prints"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    MOVReg(XRegister(2), XRegister(0)),
    LDUR(WRegister(1), XRegister(0), ImmVal(-4), Offset),
    ADR(XRegister(0), asmLocal ~ "._prints_str0"),
    BL(asmGlobal ~ "printf"),
    MOVImm(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, SP, ImmVal(16), PostIndex),
    RET
  )

  def _printb() = List(
    LabelledStringConst(asmLocal ~ "._printb_str0", "false"),
    InstrLabel("_printb"),
    STP(LR, XZR, SP, ImmVal(-16), PreIndex),
    CMPImm(WRegister(0), ImmVal(0)),
    BCond(asmGlobal ~ "L_printb0", Cond.NE),
    ADR(XRegister(2), asmLocal ~ "._preintb_str0"),
    B(asmGlobal ~ ".L_printb1")
  )

  def L_printb0() = List(
    LabelledStringConst(asmLocal ~ "._printb_str1", "true"),
    InstrLabel(".L_printb0"),
    ADR(XRegister(2), asmLocal ~ "._printb_str1")
  )

  def L_printb1() = List(
    LabelledStringConst(asmLocal ~ ".printb_str2", "%.*s"),
    InstrLabel(".L_printb1"),
    LDUR(WRegister(1), XRegister(2), ImmVal(-4), Offset),
    ADR(XRegister(0), asmLocal ~ ".printb_str2"),
    BL(asmGlobal ~ "printf"),
    MOVImm(XRegister(0), ImmVal(0)),
    BL(asmGlobal ~ "fflush"),
    LDP(LR, XZR, SP, ImmVal(16), PostIndex),
    RET
  )
}
