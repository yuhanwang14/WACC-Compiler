package wacc.backend

import java.io.File
import scala.io.Source
import scala.util.{Success, Failure}
import frontend.parser._     
import semanticCheckers._    
import backend.Generator    
import common.SymbolTable

object BackendCompiler {
  
  // Global variable to store the generated assembly code
  var outputString: String = ""

  // Exit status codes as defined by your specification
  val exitStatusSuccess = 0
  val exitStatusSyntaxError = 100
  val exitStatusSemanticError = 200
  val exitStatusFailure = -1

  /**
   * Compiles the given WACC source file.
   * @param filename the path to the WACC file.
   * @return exit code: 0 on success, 100 for syntax errors,
   *         200 for semantic errors, and -1 for file-related failures.
   */
  def compile(filename: String): Int = {
    val src = new File(filename)
    if (!src.exists() || !src.canRead) {
      println(s"Can't find or read source file at $filename")
      return exitStatusFailure
    }

    // Parse the source file
    parse(src) match {
      case Failure(ex) =>
        println(s"Parsing failed: $ex")
        return exitStatusFailure

      case Success(result) =>
        result match {
          case parsley.Success(prog) =>
            // Read the source file lines (for error messages)
            val lines = Source.fromFile(src).getLines().toSeq
            // Check semantic correctness using your ProgramChecker
            ProgramChecker.check(prog)(source = filename.split('/').last, lines = lines) match {
              case Right((newProg, symbolTable)) =>
                implicit val SymbolTable = symbolTable
                // Generate assembly code and store it in outputString
                outputString = Generator.generate(newProg).toString
                exitStatusSuccess

              case Left(errors) =>
                println("#semantic_error#")
                errors.foreach(error => println(error.format))
                exitStatusSemanticError
            }

          case parsley.Failure(error) =>
            println("#syntax_error#")
            println(error.format)
            exitStatusSyntaxError
        }
    }
  }
}
