package parsers

import AST.types.*
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.Parsley.*, parsley.Parsley
import parsley.errors.combinator.*
import parsley.expr.chain

object types_parser {
    lazy val waccType: Parsley[WACCType] = atomic(arrayType) | nonErasedPairType | baseType 
    val intType = "int" #> IntType
    val boolType = "bool" #> BoolType
    val charType = "char" #> CharType
    val stringType = "string" #> StringType
    val baseType: Parsley[WACCType] = intType | boolType | charType | stringType

    val arrayType: Parsley[WACCType] =
        chain.postfix1(baseType | nonErasedPairType)(ArrayType from "[]".hide)

    val erasedPairType = ("pair" #> ErasedPairType) <~ notFollowedBy("(")
    val pairElemType: Parsley[WACCType] = atomic(erasedPairType)| atomic(arrayType) | baseType
    lazy val nonErasedPairType: Parsley[WACCType] = 
        NonErasedPairType("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")
}
