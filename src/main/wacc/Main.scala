package wacc

import parsley.{Success, Failure}
import java.nio.file.{Files, Paths}

import parser.parse

object Main {
    val exitStatusSuccess = 0
    val exitStatusFailure = -1
    val exitStatusSyntaxError = 100
    val exitStatusSemanticError = 200

    def main(args: Array[String]): Unit = {
        println("hello WACC!")

        val fileName = args.headOption match {
            case Some(fn) => fn
            case None => {
                println("No argument given!\n" +
                    "Example usage: compile my_code.wacc")
                System.exit(exitStatusFailure)
                ""
            }
        }
        val sourceCode = try Files.readString(Paths.get(fileName)) catch {
            case _ => {
                println(s"Can't find or read source file at $fileName")
                System.exit(exitStatusFailure)
                ""
            }
        }
        parse(sourceCode) match {
            case Success(x) => println("Success.")
            case Failure(x) => println("Failure.")
        }
    }
}
