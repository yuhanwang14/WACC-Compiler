package instructions

sealed trait Instruction

case class STP(Rt: Register, Rt2: Register, Rn: Register, immVal: ImmVal, indexMode: AddressMode)
    extends Instruction {
  override def toString: String =
    s"stp ${Rt.toString}, ${Rt2.toString}, ${{ Address(Rn, indexMode, immVal) }}"
}

case class LDP(Rt: Register, Rt2: Register, Rn: Register, immVal: ImmVal, indexMode: AddressMode)
    extends Instruction {
  override def toString: String =
    s"ldp ${Rt.toString}, ${Rt2.toString}, ${{ Address(Rn, indexMode, immVal) }}"
}

case class MOVReg(Rd: Register, Rm: Register) extends Instruction {
  override def toString: String = s"mov ${Rd.toString}, ${Rm.toString}"
}

case class MOVImm(Rd: Register, immVal: ImmVal) extends Instruction {
  override def toString: String = s"mov ${Rd.toString}, ${immVal.toString}"
}

case object RET extends Instruction {
  override def toString: String = "ret"
}
