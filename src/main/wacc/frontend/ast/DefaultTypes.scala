package frontend.ast

import scala.compiletime.ops.any

object DefaultTypes {
  val defaultPos: (Int, Int) = (-1, -1)
  val anyType: WaccType = AnyType()(defaultPos)
  val intType: WaccType = IntType()(defaultPos)
  val boolType: WaccType = BoolType()(defaultPos)
  val charType: WaccType = CharType()(defaultPos)
  val stringType: WaccType = StringType()(defaultPos)
  val unknownType: WaccType = UnknownType()(defaultPos)
  val pairType: WaccType = NonErasedPairType(anyType, anyType)(defaultPos)
  def pairType(t1: WaccType, t2: WaccType): WaccType = NonErasedPairType(t1, t2)(defaultPos)
  val arrayType: WaccType = ArrayType(anyType)(defaultPos)
  def arrayType(t: WaccType): WaccType = ArrayType(t)(defaultPos)
}

export DefaultTypes.*
