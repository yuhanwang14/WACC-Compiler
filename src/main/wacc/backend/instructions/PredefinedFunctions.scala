package instructions

import instructions.Register.*

// TODO: .align 4, .word n

object PredefinedFunctions {
	val predefinedErrMessages = Map(
		"._errDivZero_str0" -> StringLabel("._errDivZero_str0", "fatal error: division or modulo by zero\n"),
		"._errOutOfMemory_str0" -> StringLabel("._errOutOfMemory_str0", "fatal error: out of memory\n"),
		"._errOverflow_str0" -> StringLabel("._errOverflow_str0", "fatal error: integer overflow or underflow occurred\n"),
		"._errNull_str0" -> StringLabel("._errNull_str0", "fatal error: null pair dereferenced or freed\n"),
		"._errOutOfBounds_str0" -> StringLabel("._errOutOfBounds_str0", "fatal error: array index %d out of bounds\n"),
	)

	def getPredefinedMessages: String = predefinedErrMessages.view.mapValues(_.toString).mkString("\n")

	def _err(localErrName: String) = List(
		InstrLabel(LocalLabel(localErrName).toString),
		ADR(XRegister(0), predefinedErrMessages.getOrElse(localErrName, StringLabel("", ""))),
		BL(GlobalLabel("_prints")),
		MOVImm(WRegister(0), ImmVal(-1)),
		BL(GlobalLabel("exit"))
	)

	def _errOutOfBounds() = _err("._errDivZero_str0")

	def _errNull() = _err("._errNull_str0")

	def _errOverflow() = _err("._errOverflow_str0")

	def _errDivZero() = _err("._errDivZero_str0")

	def _errOutOfMemory() = _err("._errOutOfMemory_str0")

	def _readc() = List(
		InstrLabel("_readc"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(1), SP),
		ADR(XRegister(0), StringLabel("._readc_str0:", "%c")),
		BL(GlobalLabel("scanf")),
		LDP(XRegister(0), LR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _readi() = List(
		InstrLabel("_readi"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(1), SP),
		ADR(XRegister(0), StringLabel("._readi_str0:", "%d")),
		BL(GlobalLabel("scanf")),
		LDP(XRegister(0), LR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _freepair() = List(
		InstrLabel("_freepair"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		CBZ(XRegister(0), GlobalLabel("_errNull")),
		BL(GlobalLabel("free")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _malloc() = List(
		InstrLabel("_malloc"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		BL(GlobalLabel("malloc")),
		CBZ(XRegister(0), GlobalLabel("_errOutOfMemory")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _printp() = List(
		InstrLabel("_printp"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(1), XRegister(0)),
		ADR(XRegister(0), StringLabel("._printp_str_0", "%p")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _println() = List(
		InstrLabel("_println"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		ADR(XRegister(0), StringLabel("._println_str_0", "")),
		BL(GlobalLabel("puts")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _printi() = List(
		InstrLabel("_printi"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(1), XRegister(0)),
		ADR(XRegister(0), StringLabel("._printi_str_0", "%d")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _printc() = List(
		InstrLabel("_printc"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(1), XRegister(0)),
		ADR(XRegister(0), StringLabel("._printc_str_0", "%c")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _prints() = List(
		InstrLabel("_prints"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(2), XRegister(0)),
		LDUR(WRegister(1), XRegister(0), ImmVal(-4), Offset),
		ADR(XRegister(0), StringLabel("._prints_str_0", "%s")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _printb() = List(
		InstrLabel("_printb"),
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		CMPImm(WRegister(0), ImmVal(0)),
		BCond(GlobalLabel("L_printb0"), Cond.NE),
		ADR(XRegister(2), StringLabel("._printb_str0", "false")),
		B(GlobalLabel(".L_printb1"))
	)

	def L_printb0() = List(
		InstrLabel(".L_printb0"),
		ADR(XRegister(2), StringLabel("._printb_str1", "true"))
	)

	def L_printb1() = List(
		InstrLabel(".L_printb1"),
		LDUR(WRegister(1), XRegister(2), ImmVal(-4), Offset),
		ADR(XRegister(0), StringLabel(".printb_str2", "%.*s")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)
}
