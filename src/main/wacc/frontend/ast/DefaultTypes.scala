package frontend.ast

object DefaultTypes {
  val defaultPos: (Int, Int) = (-1, -1)
  val anyType: WaccType = AnyType()(defaultPos)
  val intType: WaccType = IntType()(defaultPos)
  val boolType: WaccType = BoolType()(defaultPos)
  val charType: WaccType = CharType()(defaultPos)
  val stringType: WaccType = StringType()(defaultPos)
  val unknownType: WaccType = UnknownType()(defaultPos)
  val arrayType: WaccType = ArrayType(anyType)(defaultPos)
}

export DefaultTypes.*
