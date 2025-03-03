package frontend

import parser.*
import semanticCheckers.*
import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Paths}
import java.io.File
import scala.jdk.CollectionConverters.*
import scala.util.{Success, Failure}
import scala.io.Source

class FrontendSpec extends AnyFunSuite {

  // Update test lists before running
  UpdateTestFile.updateFileLists()

  val validFiles = Files.readAllLines(Paths.get("waccExamples/valid_files")).asScala.toSeq
  val invalidFiles = Files.readAllLines(Paths.get("waccExamples/invalid_files")).asScala.toSeq

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
