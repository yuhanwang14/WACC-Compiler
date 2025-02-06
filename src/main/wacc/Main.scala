package wacc

import parser.*
import semanticCheckers.*
import java.io.File
import scala.util.*
import scala.io.Source

object Main {
  val exitStatusSuccess = 0
  val exitStatusFailure = -1
  val exitStatusSyntaxError = 100
  val exitStatusSemanticError = 200

  def main(args: Array[String]): Unit = {
    val fileName = args.headOption match {
      case Some(fn) => fn
      case None => {
        println(
          "No argument given!\n" +
            "Example usage: compile my_code.wacc"
        )
        System.exit(exitStatusFailure)
        ""
      }
    }

    val src: File = new File(fileName)

    parse(src) match
      case Success(result) =>
        result match
          case parsley.Success(prog) =>
            ProgramChecker.check(prog)(
              source = fileName.split('/').last,
              lines = Source.fromFile(src).getLines().toSeq
            ) match
              case Seq() => println("Success.")
              case errors =>
                println("#semantic_error#")
                errors.map { error => println(error.format) }
                System.exit(exitStatusSemanticError)

          case parsley.Failure(error) =>
            println("#syntax_error#")
            println(error.format)
            System.exit(exitStatusSyntaxError)

      case Failure(_) =>
        println(s"Can't find or read source file at $fileName")
        System.exit(exitStatusFailure)
  }
}
