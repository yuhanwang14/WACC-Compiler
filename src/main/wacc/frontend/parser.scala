package frontend

import lexer.*
import parsers.StatementParser.*
import ast.*
import parsley.Result
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
