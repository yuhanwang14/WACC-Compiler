package parsers

import syntax.types.*
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.Parsley.*, parsley.Parsley
import parsley.expr.chain
import parsley.position.pos

object types_parser {
    lazy val waccType: Parsley[WACCType] = nonErasedPairType | arrayType | baseType 
    val intType = "int" as IntType
    val boolType = "bool" as BoolType
    val charType = "char" as CharType
    val stringType = "string" as StringType
    val baseType: Parsley[WACCType] = intType | boolType | charType | stringType

    val arrayType: Parsley[WACCType] =
        chain.postfix(baseType | nonErasedPairType) {
            (pos <* "[]").map { p => (inner: WACCType) =>
                ArrayType(inner)(p) 
            }
        }

    val erasedPairType = ("pair" as ErasedPairType) <~ notFollowedBy("(")
    val pairElemType: Parsley[WACCType] = baseType | atomic(erasedPairType) | arrayType
    lazy val nonErasedPairType: Parsley[WACCType] = 
        NonErasedPairType("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")
}