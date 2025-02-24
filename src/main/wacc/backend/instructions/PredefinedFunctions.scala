package instructions

import instructions.Register.*

object PredefinedFunctions {
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
}

/* 
// length of .L._printi_str0
	.word 2
.L._printi_str0:
	.asciz "%d"
.align 4
_printi:
	// push {lr}
	stp lr, xzr, [sp, #-16]!
	mov x1, x0
	adr x0, .L._printi_str0
	bl printf
	mov x0, #0
	bl fflush
	// pop {lr}
	ldp lr, xzr, [sp], #16
	ret
*/


