package wacc

import parsley.Result
import lexer.fully
import syntax.types.WACCType
import parsers.types_parser.waccType

object parser {
    // def parse(input: String): Result[String, Expr] = parser.parse(input)
    // private val parser = fully(expr)
    def parse(input: String): Result[String, WACCType] = parser.parse(input)
    private val parser = fully(waccType)
}
