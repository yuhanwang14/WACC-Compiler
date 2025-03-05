package backend.instructions

trait Extend(val name: String, val imm: Option[Imm] = None) extends Operand:
  override def toString: String = name ++ imm.map(i => f" $i").getOrElse("")

case class UXTB(val immVal: Option[Imm] = None) extends Extend("uxtb", immVal)
case class UXTH(val immVal: Option[Imm] = None) extends Extend("uxth", immVal)
case class LSL(val immVal: Imm) extends Extend("lsl", Some(immVal))
case class UXTX(val immVal: Option[Imm] = None) extends Extend("uxtx", immVal)
case class SXTB(val immVal: Option[Imm] = None) extends Extend("sxtb", immVal)
case class SXTH(val immVal: Option[Imm] = None) extends Extend("sxth", immVal)
case class SXTW(val immVal: Option[Imm] = None) extends Extend("sxtw", immVal)
case class SXTX(val immVal: Option[Imm] = None) extends Extend("sxtx", immVal)
