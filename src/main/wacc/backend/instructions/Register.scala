package instructions

sealed trait Register extends Operand {
  val number: Int
}

class XRegister(override val number: Int) extends Register {
  override def toString(): String = f"x$number"

  def asW: WRegister = WRegister(number)
}

class WRegister(override val number: Int) extends Register {
  override def toString(): String = f"w$number"

  def asX: XRegister = XRegister(number)
}

case object ip0 extends XRegister(16)
case object ip1 extends XRegister(17)
case object pr extends XRegister(18)

case object fp extends XRegister(29) {
  override def toString(): String = "fp"
}
case object lr extends XRegister(30) {
  override def toString(): String = "lr"
}

case object sp extends XRegister(31) {
  override def toString(): String = "sp"
}

case object xzr extends XRegister(32) {
  override def toString(): String = "xzr"
}

case object wzr extends WRegister(32) {
  override def toString(): String = "wzr"
}
