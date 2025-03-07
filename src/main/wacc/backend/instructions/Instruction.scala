package backend.instructions

sealed class Instruction(code: String) extends AsmSnippet(code)(4)

case class STP(Rt: Register, Rt2: Register, address: Address)
    extends Instruction(f"stp $Rt, $Rt2, $address\n")

case class LDP(Rt: Register, Rt2: Register, address: Address)
    extends Instruction(f"ldp $Rt, $Rt2, $address\n")

case class LDR(Rt: Register, address: Address) extends Instruction(f"ldr $Rt, $address\n")

case class LDRB(Rt: Register, address: Address) extends Instruction(f"ldrb $Rt, $address\n")

case class LDUR(Rt: Register, address: Address) extends Instruction(f"ldur $Rt, $address\n")

case class LDURB(Rt: Register, address: Address) extends Instruction(f"ldurb $Rt, $address\n")

case class STR(Rt: Register, address: Address) extends Instruction(f"str $Rt, $address\n")

case class STRB(Rt: Register, address: Address) extends Instruction(f"strb $Rt, $address\n")

case class STUR(Rt: Register, address: Address) extends Instruction(f"stur $Rt, $address\n")

case class STURB(Rt: Register, address: Address) extends Instruction(f"sturb $Rt, $address\n")

case class MOV(Rd: Register, opr: Operand) extends Instruction(f"mov $Rd, $opr\n")

case class MOVK(Rd: Register, opr: Operand, extend: Option[Extend])
    extends Instruction(f"movk $Rd, $opr" + extend.map(e => s", $e\n").getOrElse("\n"))

case class CMP(Rd: Register, opr: Operand, extend: Option[Extend] = None)
    extends Instruction(f"cmp $Rd, $opr" + extend.map(e => s", $e\n").getOrElse("\n"))

case class CSET(Rd: Register, cond: Cond) extends Instruction(f"cset $Rd, $cond\n")

case class CSEL(Rd: Register, Rn: Register, Rm: Register, cond: Cond)
    extends Instruction(f"csel $Rd, $Rn, $Rm, $cond\n")

case class ADD(Rd: Register, Rn: Register, opr: Operand) extends Instruction(f"add $Rd, $Rn, $opr\n")

case class ADDS(Rd: Register, Rn: Register, opr: Operand)
    extends Instruction(f"adds $Rd, $Rn, $opr\n")

case class SUB(Rd: Register, Rn: Register, opr: Operand) extends Instruction(f"sub $Rd, $Rn, $opr\n")

case class SUBS(Rd: Register, Rn: Register, opr: Operand)
    extends Instruction(f"subs $Rd, $Rn, $opr\n")

case class SMULL(Rd: Register, Rn: Register, opr: Operand)
    extends Instruction(f"smull $Rd, $Rn, $opr\n")

case class SDIV(Rd: Register, Rn: Register, opr: Operand)
    extends Instruction(f"sdiv $Rd, $Rn, $opr\n")

case class MSUB(Rd: Register, Rn: Register, Rm: Register, Ra: Register)
    extends Instruction(f"msub $Rd, $Rn, $Rm, $Ra\n")

case class TST(Rd: Register, opr: Operand) extends Instruction(f"tst $Rd, $opr\n")

case class NEGS(Rd: Register, Rn: Register) extends Instruction(f"negs $Rd, $Rn\n")

case class CBNZ(Rd: Register, label: String) extends Instruction(f"cbnz $Rd, $label\n")

case class CBZ(Rd: Register, label: String) extends Instruction(f"cbz $Rd, $label\n")

case class ADR(Rd: Register, label: String) extends Instruction(f"adr $Rd, $label\n")

case class ADRP(Rd: Register, label: String) extends Instruction(f"adrp $Rd, $label\n")

case class B(label: String) extends Instruction(f"b $label\n")

case class BL(label: String) extends Instruction(f"bl $label\n")

case class BCond(label: String, cond: Cond) extends Instruction(f"b.$cond $label\n")

case object RET extends Instruction("ret\n")
