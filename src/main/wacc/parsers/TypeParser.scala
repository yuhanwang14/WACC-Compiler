package parsers

import ast.*
import wacc.lexer.*
import wacc.lexer.implicits.implicitSymbol
import parsley.Parsley.*, parsley.Parsley
import parsley.expr.*
import parsley.errors.combinator.*
import parsley.errors.patterns.*

object TypeParser {
  lazy val waccType: Parsley[WaccType] =
    (arrayType | nonErasedPairType | baseType).label("type")
  val intType = IntType from "int"
  val boolType = BoolType from "bool"
  val charType = CharType from "char"
  val stringType = StringType from "string"
  val baseType: Parsley[WaccType] = intType | boolType | charType | stringType

  val arrayType: Parsley[WaccType] =
    atomic(
      chain.postfix1(baseType | nonErasedPairType)(
        ArrayType from "[]".label("array type")
      )
    )

  val erasedPairType = (ErasedPairType from "pair") <~ "(".preventativeExplain(
    reason = "pair types may not be nested in WACC"
  )
  val pairElemType: Parsley[WaccType] = erasedPairType | arrayType | baseType
  lazy val nonErasedPairType: Parsley[WaccType] =
    NonErasedPairType(
      "pair" ~> "(" ~> pairElemType <~ ",",
      pairElemType <~ ")"
    )
}
