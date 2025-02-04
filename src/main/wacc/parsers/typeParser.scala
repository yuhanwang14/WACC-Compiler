package parsers

import AST.types.*
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.Parsley.*, parsley.Parsley
import parsley.expr.chain

object typeParser {
    lazy val waccType: Parsley[WACCType] = arrayType | nonErasedPairType | baseType 
    val intType = IntType from "int"
    val boolType = BoolType from "bool"
    val charType = CharType from "char"
    val stringType = StringType from "string"
    val baseType: Parsley[WACCType] = intType | boolType | charType | stringType

    val arrayType: Parsley[WACCType] =
        atomic(chain.postfix1(baseType | nonErasedPairType)(ArrayType from "[]"))

    val erasedPairType = (ErasedPairType from "pair") <~ notFollowedBy("(")
    val pairElemType: Parsley[WACCType] = erasedPairType | arrayType | baseType
    lazy val nonErasedPairType: Parsley[WACCType] = 
        NonErasedPairType("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")
}
