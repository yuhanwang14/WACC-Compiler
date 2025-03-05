package testcode

import frontend.parser.*
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import scala.jdk.CollectionConverters.*
import scala.util.{Success, Failure}
import scala.io.Source
import semanticCheckers.ProgramChecker

class FrontendSpec extends AnyFunSuite {

  // Update test lists before running
  TestFiles.updateFileLists()

  val validFiles = TestFiles.validFiles
  val invalidFiles = TestFiles.invalidFiles

  def testFile(fileName: String, shouldPass: Boolean): Unit = {
    val src = new File(fileName)

    parse(src) match {
      case Success(result) =>
        result match {
          case parsley.Success(prog) =>
            implicit val lines: Seq[String] = Source.fromFile(src).getLines().toSeq
            implicit val sourceName: String = fileName

            ProgramChecker.check(prog) match {
              case Right(_) if shouldPass =>
                assert(true)

              case Right(_) =>
                println(s"Test Failed (Expected Semantic Error but Passed): $fileName")
                assert(false)

              case Left(errors) if !shouldPass =>
                assert(true)

              case Left(errors) =>
                println(s"Unexpected Semantic Errors in $fileName")
                assert(false)
            }

          case parsley.Failure(error) =>
            assert(!shouldPass)
        }

      case Failure(_) =>
        println(s"Can't find or read source file at $fileName")
        assert(false)
    }
  }

  validFiles.foreach { fileName =>
    test(s"Valid Test: $fileName") { testFile(fileName, shouldPass = true) }
  }

  invalidFiles.foreach { fileName =>
    test(s"Invalid Test: $fileName") { testFile(fileName, shouldPass = false) }
  }
}
