package backend

import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process._
import java.nio.file.{Files, Paths}
import java.io.File
import scala.io.Source

class BackendSpec extends AnyFunSuite {

  // Update test lists before running
  UpdateTestFile.updateFileLists()

  val fileList = Files.readAllLines(Paths.get("waccExamples/valid_files")).asScala.toSeq
  val qemuPath = "/usr/aarch64-linux-gnu/"

  fileList.foreach { fileName =>
    test(s"Backend Test: $fileName") {
      val asmFile = fileName.stripSuffix(".wacc") + ".s"
      val exeFile = "program"
      val expectedOutputFile = fileName.stripSuffix(".wacc") + ".expected"

      val compileCmd = s"./compile $fileName"
      assert(compileCmd.! == 0, s"Compilation failed for $fileName")

      val assembleCmd = s"aarch64-linux-gnu-gcc -o $exeFile -z noexecstack -march=armv8-a $asmFile"
      assert(assembleCmd.! == 0, s"Assembly failed for $asmFile")

      val actualOutput = s"qemu-aarch64 -L $qemuPath $exeFile".!!
      val expectedOutput = Source.fromFile(expectedOutputFile).getLines().mkString("\n")

      assert(actualOutput.trim == expectedOutput.trim, s"Output mismatch for $fileName")
    }
  }
}
