package instructions

sealed trait Register {
  def number: Int
}

object Register {
  val IP0: XRegister = XRegister(16)
  val IP1: XRegister = XRegister(17)
  val PR:  XRegister = XRegister(18)
  
  val FP:  XRegister = XRegister(29) 
  val LR:  XRegister = XRegister(30)
  val SP:  XRegister = XRegister(31) 
  val XZR: XRegister = XRegister(31, isZero = true)  // Zero Register
}

final case class XRegister(number: Int, isZero: Boolean = false) extends Register {
  def asW: WRegister = WRegister(number, isZero)

  override def toString: String = (number, isZero) match {
    case (29, _) => "fp"  
    case (30, _) => "lr"  
    case (31, false) => "sp" 
    case (31, true) => "xzr"
    case _  => s"x$number"
  }
}

final case class WRegister(number: Int, isZero: Boolean = false) extends Register {
  def asX: XRegister = XRegister(number, isZero)

  override def toString: String = (number, isZero) match {
    case (29, _) => "wfp" 
    case (30, _) => "wlr" 
    case (31, false) => "sp"  
    case (31, ture) => "wzr"
    case _  => s"w$number"
  }
}
