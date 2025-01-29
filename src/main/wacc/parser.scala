package wacc

import parsley.Result
import lexer.fully
import parsers.statements_parser.program
import AST.statements.Program

object parser {
    def parse(input: String): Result[String, Program] = parser.parse(input)
    private val parser = fully(program)
}
