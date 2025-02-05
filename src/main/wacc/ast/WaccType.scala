package ast

import utils.*

sealed trait WaccType extends Position
case class AnyType()(val pos: (Int, Int)) extends WaccType {
    override def toString(): String = "any type"
}
case class UnknownType()(val pos: (Int, Int)) extends WaccType {
    override def toString(): String = "unknown type"
}

case class FirstErrorType(t: WaccType)(val pos: (Int, Int)) extends WaccType
case class SecondErrorType(t: WaccType)(val pos: (Int, Int)) extends WaccType

// Base Type
sealed trait BaseType extends WaccType
case class IntType()(val pos: (Int, Int)) extends BaseType {
    override def toString(): String = "int"
}
case class BoolType()(val pos: (Int, Int)) extends BaseType {
    override def toString(): String = "bool"
}
case class CharType()(val pos: (Int, Int)) extends BaseType {
    override def toString(): String = "char"
}
case class StringType()(val pos: (Int, Int)) extends BaseType {
    override def toString(): String = "string"
}
// Array Type
case class ArrayType(t: WaccType)(val pos: (Int, Int)) extends WaccType {
    override def toString(): String = s"array<${t}>"
}
// Pair Type
case class ErasedPairType()(val pos: (Int, Int)) extends WaccType {
    override def toString(): String = "pair"
}
case class NonErasedPairType(t1: WaccType, t2: WaccType)(val pos: (Int, Int)) extends WaccType {
    override def toString(): String = s"pair<${t1}, ${t2}>"
}

object AnyType extends ParserBridgePos0[AnyType]
object UnknownType extends ParserBridgePos0[UnknownType]
object FirstErrorType extends ParserBridgePos1[WaccType, FirstErrorType]
object SecondErrorType extends ParserBridgePos1[WaccType, SecondErrorType]

object IntType extends ParserBridgePos0[IntType]
object BoolType extends ParserBridgePos0[BoolType]
object CharType extends ParserBridgePos0[CharType]
object StringType extends ParserBridgePos0[StringType]

object ArrayType extends ParserBridgePos1[WaccType, ArrayType]
object ErasedPairType extends ParserBridgePos0[ErasedPairType]
object NonErasedPairType extends ParserBridgePos2[WaccType, WaccType, NonErasedPairType]
