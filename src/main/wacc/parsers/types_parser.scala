package parsers

import syntax.types.*
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.Parsley.*, parsley.Parsley
import parsley.expr.chain

object types_parser {
    lazy val waccType: Parsley[WACCType] = atomic(arrayType) | nonErasedPairType | baseType 
    val intType = "int" as IntType
    val boolType = "bool" as BoolType
    val charType = "char" as CharType
    val stringType = "string" as StringType
    val baseType: Parsley[WACCType] = intType | boolType | charType | stringType

    val arrayType: Parsley[WACCType] =
        chain.postfix1(baseType | nonErasedPairType)(ArrayType from "[]")

    val erasedPairType = ("pair" as ErasedPairType) <~ notFollowedBy("(")
    val pairElemType: Parsley[WACCType] = atomic(erasedPairType)| atomic(arrayType) | baseType
    lazy val nonErasedPairType: Parsley[WACCType] = 
        NonErasedPairType("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")
}
