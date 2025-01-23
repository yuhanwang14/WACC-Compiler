package syntax

import utils.ParserBridgePos1
import utils.ParserBridgePos2

object types {
    sealed trait WACCType
    // Base Type
    case object IntType extends WACCType
    case object BoolType extends WACCType
    case object CharType extends WACCType
    case object StringType extends WACCType
    // Array Type
    case class ArrayType(t: WACCType)(val pos: (Int, Int)) extends WACCType
    // Pair Type
    case object ErasedPairType extends WACCType
    case class NonErasedPairType(t1: WACCType, t2: WACCType)(val pos: (Int, Int)) extends WACCType

    object ArrayType extends ParserBridgePos1[WACCType, ArrayType]
    object NonErasedPairType extends ParserBridgePos2[WACCType, WACCType, NonErasedPairType]
}