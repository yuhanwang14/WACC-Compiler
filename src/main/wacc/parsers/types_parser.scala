package parsers

import AST.types.*
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.Parsley.*, parsley.Parsley
import parsley.errors.combinator.*
import parsley.errors.patterns.*
import parsley.expr.chain

object types_parser {
    lazy val waccType: Parsley[WACCType] = arrayType | nonErasedPairType | baseType 
    val intType = "int" #> IntType
    val boolType = "bool" #> BoolType
    val charType = "char" #> CharType
    val stringType = "string" #> StringType
    val baseType: Parsley[WACCType] = intType | boolType | charType | stringType

    val arrayType: Parsley[WACCType] =
        atomic(chain.postfix1(baseType | nonErasedPairType)(ArrayType from "[]"))

    val erasedPairType = ("pair" #> ErasedPairType) <~ notFollowedBy("(")
    val pairElemType: Parsley[WACCType] = erasedPairType | arrayType | baseType
    lazy val nonErasedPairType: Parsley[WACCType] = 
        NonErasedPairType("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")
}
