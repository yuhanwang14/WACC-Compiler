package instructions

sealed trait Register extends Operand {
  def number: Int
}

object Register {
  val IP0: XRegister = XRegister(16)
  val IP1: XRegister = XRegister(17)
  val PR:  XRegister = XRegister(18)
  
  val FP:  XRegister = XRegister(29) 
  val LR:  XRegister = XRegister(30)
  val SP:  XRegister = XRegister(31) 
  val XZR: XRegister = XRegister(32)  // Zero Register
}

final case class XRegister(number: Int) extends Register {
  def asW: WRegister = WRegister(number)

  override def toString: String = number match {
    case 29 => "fp"
    case 30 => "lr"
    case 31 => "sp"
    case 32 => "xzr"
    case _  => s"x$number"
  }
}

final case class WRegister(number: Int) extends Register {
  def asX: XRegister = XRegister(number)

  override def toString: String = number match {
    case 29 => "wfp"
    case 30 => "wlr"
    case 31 => "sp"
    case 32 => "wzr"
    case _  => s"w$number"
  }
}
