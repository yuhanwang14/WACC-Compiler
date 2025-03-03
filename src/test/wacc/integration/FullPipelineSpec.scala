package integration

import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process._
import java.nio.file.{Files, Paths}

class FullPipelineSpec extends AnyFunSuite {

  // Update test lists before running
  UpdateTestFile.updateFileLists()

  val validFiles = Files.readAllLines(Paths.get("waccExamples/valid_files")).asScala.toSeq
  val invalidFiles = Files.readAllLines(Paths.get("waccExamples/invalid_files")).asScala.toSeq

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
