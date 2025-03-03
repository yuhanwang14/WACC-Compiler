package testcode

import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process._
import java.nio.file.{Files, Paths}
import test.wacc.TestFiles

class FullPipelineSpec extends AnyFunSuite {

  // Update test lists before running
  UpdateTestFile.updateFileLists()

  val validFiles = TestFiles.validFiles
  val invalidFiles = TestFiles.validFiles

  test("Run all valid and invalid WACC programs") {
    validFiles.foreach { file =>
      assert(s"./compile $file".! == 0, s"Compilation failed for $file")
    }

    invalidFiles.foreach { file =>
      val exitCode = s"./compile $file".!
      assert(exitCode == 100 || exitCode == 200, s"Invalid file passed compilation: $file")
    }
  }
}
