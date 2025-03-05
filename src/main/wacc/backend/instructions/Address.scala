package backend.instructions

trait Operand

trait Imm extends Operand

case class ImmVal(value: Int) extends Imm:
  override def toString: String = f"#${value}"

case class Lo12(label: String) extends Imm:
  override def toString: String = f":lo12:$label"

trait Address(override val toString: String) extends Operand

case class Offset(reg: Register, offset: Imm) extends Address(f"[$reg, $offset]")

case class PreIndex(reg: Register, offset: Imm) extends Address(f"[$reg, $offset]!")

case class PostIndex(reg: Register, offset: Imm) extends Address(f"[$reg], $offset")

case class RegisterAddress(Rn: Register, Rm: Register, extend: Option[Extend] = None)
    extends Address(f"[$Rn, $Rm" + extend.map(e => s", $e").getOrElse("") + "]")
