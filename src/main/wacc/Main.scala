package wacc

import scala.util.{Success, Failure}
import wacc.backend.BackendCompiler
import common.FileUtil

object Main {
  val exitStatusSuccess       = 0
  val exitStatusFailure       = -1
  val exitStatusSyntaxError   = 100
  val exitStatusSemanticError = 200

  def main(args: Array[String]): Unit = {
    // Ensure a filename is provided.
    val fileName = args.headOption.getOrElse {
      println("No argument given!\nExample usage: ./compile {my_code}.wacc")
      sys.exit(exitStatusFailure)
      ""
    }

    // Call BackendCompiler.compile, which now returns Either[Int, String]
    val compileResult = BackendCompiler.compile(fileName)

    compileResult match {
      case Right(asmString) =>
        // Compilation succeeded. Write the assembly code to a file.
        val baseName = fileName.substring(fileName.lastIndexOf('/') + 1).replace(".wacc", ".s")
        FileUtil.writeFile(baseName, asmString) match {
          case scala.util.Success(_) =>
            println(s"Assembly code written to $baseName")
          case scala.util.Failure(e) =>
            println(s"Failed to write assembly file: ${e.getMessage}")
            sys.exit(exitStatusFailure)
        }
        sys.exit(exitStatusSuccess)
      case Left(exitCode) =>
        // Compilation failed with a specific exit code.
        sys.exit(exitCode)
    }
  }
}
