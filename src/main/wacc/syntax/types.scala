package syntax

import utils.ParserBridgePos1
import utils.ParserBridgePos2

object types {
    sealed trait WACCType
    // Base Type
    sealed trait BaseType extends WACCType
    case object IntType extends BaseType
    case object BoolType extends BaseType
    case object CharType extends BaseType
    case object StringType extends BaseType
    // Array Type
    case class ArrayType(t: WACCType)(val pos: (Int, Int)) extends WACCType
    // Pair Type
    case object ErasedPairType extends WACCType
    case class NonErasedPairType(t1: WACCType, t2: WACCType)(val pos: (Int, Int)) extends WACCType

    object ArrayType extends ParserBridgePos1[WACCType, ArrayType]
    object NonErasedPairType extends ParserBridgePos2[WACCType, WACCType, NonErasedPairType]
}