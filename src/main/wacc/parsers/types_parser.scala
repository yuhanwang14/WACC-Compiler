package parsers

import syntax.types.*
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.Parsley.*, parsley.Parsley

object types_parser {
    lazy val waccType: Parsley[WACCType] = baseType | arrayType | nonErasedPairType
    lazy val intType = "int" as IntType
    lazy val boolType = "bool" as BoolType
    lazy val charType = "char" as CharType
    lazy val stringType = "string" as StringType
    lazy val baseType: Parsley[WACCType] = intType | boolType | charType | stringType

    lazy val arrayType: Parsley[WACCType] = atomic(ArrayType(waccType <~ "[]"))

    lazy val erasedPairType = "pair" as ErasedPairType
    lazy val pairElemType: Parsley[WACCType] = baseType | arrayType | erasedPairType
    lazy val nonErasedPairType: Parsley[WACCType] = 
        atomic(NonErasedPairType("pair" ~> "(" ~> pairElemType,  "," ~> pairElemType <~ ")"))
}