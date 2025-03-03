package instructions

sealed trait AddressMode
case object Offset extends AddressMode
case object PreIndex extends AddressMode
case object PostIndex extends AddressMode

trait Operand

case class ImmVal(value: Int) extends Operand {
  override def toString: String = s"#${value}"
}

trait Address(override val toString: String)

case class Offset(reg: Register, offset: ImmVal) extends Address(f"[$reg, $offset]")

case class PreIndex(reg: Register, offset: ImmVal) extends Address(f"[$reg, $offset]!")

case class PostIndex(reg: Register, offset: ImmVal) extends Address(f"[$reg], $offset")

case class _Address(
    reg: Register,
    indexMode: AddressMode = Offset,
    immVal: ImmVal
) {
  override def toString: String = indexMode match
    case Offset    => s"[${reg.toString}, ${immVal.toString}]"
    case PreIndex  => s"[${reg.toString}, ${immVal.toString}]!"
    case PostIndex => s"[${reg.toString}], ${immVal.toString}"
}
