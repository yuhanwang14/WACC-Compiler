package instructions

sealed trait AddressMode
case object Offset extends AddressMode
case object PreIndex extends AddressMode
case object PostIndex extends AddressMode

trait Operand

case class ImmVal(value: Int) extends Operand {
  override def toString: String = s"#${value}"
}

case class Address(
    reg: Register,
    indexMode: AddressMode = Offset,
    immVal: ImmVal
) {
  override def toString: String = indexMode match
    case Offset    => s"[${reg.toString}, ${immVal.toString}]"
    case PreIndex  => s"[${reg.toString}, ${immVal.toString}]!"
    case PostIndex => s"[${reg.toString}], ${immVal.toString}"
}
