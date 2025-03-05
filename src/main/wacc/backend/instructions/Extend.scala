package backend.instructions

trait Extend(val name: String, val imm: Option[Imm] = None) extends Operand:
  override def toString: String = name ++ imm.map(i => f" $i").getOrElse("")

case class UXTB(override val imm: Option[Imm] = None) extends Extend("uxtb", imm)
case class UXTH(override val imm: Option[Imm] = None) extends Extend("uxth", imm)
case class LSL(override val imm: Imm) extends Extend("lsl", Some(imm))
case class UXTX(override val imm: Option[Imm] = None) extends Extend("uxtx", imm)
case class SXTB(override val imm: Option[Imm] = None) extends Extend("sxtb", imm)
case class SXTH(override val imm: Option[Imm] = None) extends Extend("sxth", imm)
case class SXTW(override val imm: Option[Imm] = None) extends Extend("sxtw", imm)
case class SXTX(override val imm: Option[Imm] = None) extends Extend("sxtx", imm)
