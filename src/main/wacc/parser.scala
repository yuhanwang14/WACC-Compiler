package wacc

import parsley.Result
import lexer.fully
import parsers.StatementParser.program
import ast.Program

import errors.{CustomParsleyErrorBuilder, Error}
import java.io.File
import scala.util.Try

object parser {
    implicit val errorBuilder: CustomParsleyErrorBuilder =
        new CustomParsleyErrorBuilder
    def parse(input: File): Try[Result[Error, Program]] =
        parser.parseFile(input)
    private val parser = fully(program)
}
