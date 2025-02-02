package AST

import utils.*
import AST.position.Position

object types {
    sealed trait WACCType extends Position
    case class AnyType()(val pos: (Int, Int)) extends WACCType
    case class UnknownType()(val pos: (Int, Int)) extends WACCType

    // Base Type
    sealed trait BaseType extends WACCType
    case class IntType()(val pos: (Int, Int)) extends BaseType
    case class BoolType()(val pos: (Int, Int)) extends BaseType
    case class CharType()(val pos: (Int, Int)) extends BaseType
    case class StringType()(val pos: (Int, Int)) extends BaseType
    // Array Type
    case class ArrayType(t: WACCType)(val pos: (Int, Int)) extends WACCType
    // Pair Type
    case class ErasedPairType()(val pos: (Int, Int)) extends WACCType
    case class NonErasedPairType(t1: WACCType, t2: WACCType)(val pos: (Int, Int)) extends WACCType

    object AnyType extends ParserBridgePos0[AnyType]
    object UnknownType extends ParserBridgePos0[UnknownType]
    object IntType extends ParserBridgePos0[IntType]
    object BoolType extends ParserBridgePos0[BoolType]
    object CharType extends ParserBridgePos0[CharType]
    object StringType extends ParserBridgePos0[StringType]

    object ArrayType extends ParserBridgePos1[WACCType, ArrayType]
    object ErasedPairType extends ParserBridgePos0[ErasedPairType]
    object NonErasedPairType extends ParserBridgePos2[WACCType, WACCType, NonErasedPairType]
}