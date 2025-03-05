package wacc

import scala.util.{Success, Failure}
import backend.BackendCompiler
import common.FileUtil

object Main {
  val exitStatusSuccess       = 0
  val exitStatusFailure       = -1
  val exitStatusSyntaxError   = 100
  val exitStatusSemanticError = 200

  def main(args: Array[String]): Unit = {
    // Ensure a filename is provided.
    val fileName = args.headOption.getOrElse {
      println("No argument given!\nExample usage: ./wacc-compiler {my_code}.wacc") 
      sys.exit(exitStatusFailure)
      ""
    }

    // Call BackendCompiler.compile, returns an exit code.
    val exitCode = BackendCompiler.compile(fileName)

    if (exitCode == exitStatusSuccess) {
      // Extract a base name for the output file.
      val baseName = fileName.substring(fileName.lastIndexOf('/') + 1).replace(".wacc", ".s")
      
      // Use Using to safely write the output.
      FileUtil.writeFile(baseName, BackendCompiler.outputString) match {
        case Success(_) =>
          println(s"Assembly code written to $baseName")
        case Failure(ex) =>
          println(s"Failed to write assembly file: ${ex.getMessage}")
          sys.exit(exitStatusFailure)
      }
    }
    sys.exit(exitCode)
  }
}
