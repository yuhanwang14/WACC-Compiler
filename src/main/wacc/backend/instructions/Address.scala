package instructions

trait Operand

case class ImmVal(value: Int) extends Operand {
  override def toString: String = s"#${value}"
}

trait Address(override val toString: String)

case class Offset(reg: Register, offset: ImmVal) extends Address(f"[$reg, $offset]")

case class PreIndex(reg: Register, offset: ImmVal) extends Address(f"[$reg, $offset]!")

case class PostIndex(reg: Register, offset: ImmVal) extends Address(f"[$reg], $offset")
