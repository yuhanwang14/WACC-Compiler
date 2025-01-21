package wacc

import parsley.Result

import lexer.fully
import expressions_parser.*
import expressions.*

object parser {
    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)
}
