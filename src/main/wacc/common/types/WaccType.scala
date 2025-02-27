package common.types

sealed trait WaccType(override val toString: String)(override val byteSize: Int) extends Sizeable

case class AnyType() extends WaccType("any type")(0)

case class UnknownType() extends WaccType("unknown type")(0)

case class IntType() extends WaccType("int")(4)

case class BoolType() extends WaccType("bool")(1)

case class CharType() extends WaccType("char")(1)

case class StringType() extends WaccType("string")(8)

case class ArrayType(t: WaccType) extends WaccType(s"array<${t}>")(8)

case class PairType(t1: WaccType, t2: WaccType) extends WaccType(s"pair<${t1}, ${t2}>")(8)
