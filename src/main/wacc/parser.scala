package wacc

import parsley.Result
import lexer.fully
import parsers.statementParser.program
import ast.statements.Program

import errors.errors.{CustomErrorBuilder, Error}
import java.io.File
import scala.util.Try

object parser {
    implicit val errorBuilder: CustomErrorBuilder = new CustomErrorBuilder
    def parse(input: File): Try[Result[Error, Program]] = parser.parseFile(input)
    private val parser = fully(program)
}
