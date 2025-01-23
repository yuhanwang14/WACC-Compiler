package wacc

import parsley.Result
import lexer.fully
import parsers.expressions_parser.*
import syntax.expressions.*

object parser {
    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)
}
