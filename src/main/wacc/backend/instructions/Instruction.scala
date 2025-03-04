package instructions

sealed class Instruction(code: String) extends AsmSnippet(code)(4)

case class STP(Rt: Register, Rt2: Register, address: Address)
    extends Instruction(f"stp $Rt, $Rt2, $address")

case class LDP(Rt: Register, Rt2: Register, address: Address)
    extends Instruction(f"ldp $Rt, $Rt2, $address")

case class LDUR(Rt: Register, address: Address) extends Instruction(f"ldur $Rt, $address")

case class LDURB(Rt: Register, address: Address) extends Instruction(f"ldurb $Rt, $address")

case class STR(Rt: Register, address: Address) extends Instruction(f"str $Rt, $address")

case class STUR(Rt: Register, address: Address) extends Instruction(f"stur $Rt, $address")

case class STURB(Rt: Register, address: Address) extends Instruction(f"sturb $Rt, $address")

case class MOV(Rd: Register, opr: Operand) extends Instruction(f"mov $Rd, $opr")

case class CMP(Rd: Register, opr: Operand) extends Instruction(f"cmp $Rd, $opr")

case class CSET(Rd: Register, cond: Cond) extends Instruction(f"cset $Rd, $cond")

case class ADD(Rd: Register, Rn: Register, opr: Operand) extends Instruction(f"add $Rd, $Rn, $opr")

case class ADDS(Rd: Register, Rn: Register, opr: Operand)
    extends Instruction(f"adds $Rd, $Rn, $opr")

case class SUBS(Rd: Register, Rn: Register, opr: Operand)
    extends Instruction(f"subs $Rd, $Rn, $opr")

case class CBZ(Rd: Register, label: String) extends Instruction(f"cbz $Rd, $label")

case class ADR(Rd: Register, label: String) extends Instruction(f"adr $Rd, $label")

case class ADRP(Rd: Register, label: String) extends Instruction(f"adrp $Rd, $label")

case class B(label: String) extends Instruction(f"b $label")

case class BL(label: String) extends Instruction(f"bl $label")

case class BCond(label: String, cond: Cond) extends Instruction(f"b.$cond 2$label")

case object RET extends Instruction("ret")

case class ALIGN(power: Int) extends Instruction(f".align $power")

case class WORD(len: Int) extends Instruction(f".word $len")
