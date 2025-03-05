package backend.instructions

sealed trait Register extends Operand:
  val number: Int
  def asW: WRegister = WRegister(number)
  def asX: XRegister = XRegister(number)

class XRegister(override val number: Int) extends Register:
  override def toString: String = f"x$number"

class WRegister(override val number: Int) extends Register:
  override def toString: String = f"w$number"

case object x0 extends XRegister(0)
case object x1 extends XRegister(1)
case object x2 extends XRegister(2)
case object x8 extends XRegister(8)
case object x9 extends XRegister(9)
case object x10 extends XRegister(10)


case object ip0 extends XRegister(16)
case object ip1 extends XRegister(17)
case object pr extends XRegister(18)

case object fp extends XRegister(29):
  override def toString: String = "fp"

case object lr extends XRegister(30):
  override def toString: String = "lr"

case object sp extends XRegister(31):
  override def toString: String = "sp"

case object xzr extends XRegister(32):
  override def toString: String = "xzr"

case object w0 extends WRegister(0)
case object w1 extends WRegister(1)
case object w8 extends WRegister(8)
case object w9 extends WRegister(9)
case object w10 extends WRegister(10)
case object w11 extends WRegister(11)

case object wzr extends WRegister(32) {
  override def toString(): String = "wzr"
}

