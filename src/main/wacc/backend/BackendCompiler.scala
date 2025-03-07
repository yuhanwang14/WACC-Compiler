package wacc.backend

import java.io.File
import scala.io.Source
import scala.util.{Success, Failure}
import frontend.parser.*
import backend.Generator    
import common.SymbolTable
import semanticCheckers.ProgramChecker

object BackendCompiler {
  
  // Exit status codes as defined by your specification
  val exitStatusSuccess     = 0
  val exitStatusSyntaxError = 100
  val exitStatusSemanticError = 200
  val exitStatusFailure     = -1


  /**
   * Compiles the given WACC source file.
   * @param filename the path to the WACC file.
   * @return Either the exit code (Left) or the generated assembly code as a String (Right).
   */
  def compile(filename: String): Either[Int, String] = {
    val src = new File(filename)

    if (!src.exists() || !src.canRead) {
      println(s"Can't find or read source file at $filename")
      return Left(exitStatusFailure)
    }

    // Parse the source file
    parse(src) match {
      case Failure(ex) =>
        println(s"Parsing failed: $ex")
        Left(exitStatusFailure)

      case Success(result) =>
        result match {
          case parsley.Success(prog) =>
            // Read the source file lines (for error messages)
            val lines = Source.fromFile(src).getLines().toSeq
            // Check semantic correctness using your ProgramChecker
            ProgramChecker.check(prog)(source = filename.split('/').last, lines = lines) match {
              case Right((newProg, symbolTable)) =>
                // Generate assembly code and return it
                val asmString = Generator(newProg)(symbolTable.toFrozen).generate
                Right(asmString)


              case Left(errors) =>
                println("#semantic_error#")
                errors.foreach(error => println(error.format))
                Left(exitStatusSemanticError)
            }

          case parsley.Failure(error) =>
            println("#syntax_error#")
            println(error.format)
            Left(exitStatusSyntaxError)
        }
    }
  }
}
