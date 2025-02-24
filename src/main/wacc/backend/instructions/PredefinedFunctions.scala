package instructions

import instructions.Register.*

object PredefinedFunctions {
	def _malloc() = List(
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		BL(GlobalLabel("malloc")),
		CBZ(XRegister(0), GlobalLabel("_errOutOfMemory")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _errOutOfMemory() = List(
		ADR(XRegister(0), LocalLabel("._errOutOfMemory_str0")),
		BL(GlobalLabel("_prints")),
		MOVImm(WRegister(0), ImmVal(-1)),
		BL(GlobalLabel("exit"))
	)

	def _printp() = List(
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(1), XRegister(0)),
		ADR(XRegister(0), LocalLabel("._printp_str_0")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _println() = List(
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		ADR(XRegister(0), LocalLabel("._println_str_0")),
		BL(GlobalLabel("puts")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _printi() = List(
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(1), XRegister(0)),
		ADR(XRegister(0), LocalLabel("._printi_str_0")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _printc() = List(
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(1), XRegister(0)),
		ADR(XRegister(0), LocalLabel("._printc_str_0")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _prints() = List(
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		MOVReg(XRegister(2), XRegister(0)),
		LDUR(WRegister(1), XRegister(0), ImmVal(-4), Offset),
		ADR(XRegister(0), LocalLabel("._prints_str_0")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)

	def _printb() = List(
		STP(LR, XZR, SP, ImmVal(-16), PreIndex),
		CMPImm(WRegister(0), ImmVal(0)),
		BCond(GlobalLabel("L_printb0"), Cond.NE),
		ADR(XRegister(2), LocalLabel("._printb_str0")),
		B(GlobalLabel(".L_printb1"))
	)

	def L_printb0() = List(
		ADR(XRegister(2), LocalLabel("._printb_str1"))
	)

	def L_printb1() = List(
		LDUR(WRegister(1), XRegister(2), ImmVal(-4), Offset),
		ADR(XRegister(0), LocalLabel("._printb_str2")),
		BL(GlobalLabel("printf")),
		MOVImm(XRegister(0), ImmVal(0)),
		BL(GlobalLabel("fflush")),
		LDP(LR, XZR, SP, ImmVal(16), PostIndex),
		RET
	)
}


