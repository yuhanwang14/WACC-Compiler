package testcode

import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process._
import java.io.File
import scala.io.Source

class BackendSpec extends AnyFunSuite {

  // Update test lists before running
  TestFiles.updateFileLists()

  val validFiles = TestFiles.validFiles
  val invalidFiles = TestFiles.invalidFiles

  val qemuPath = "/usr/aarch64-linux-gnu/"

  def testFile(fileName: String): Unit = test(s"Backend Test: $fileName") {
      val asmFile = fileName.stripSuffix(".wacc") + ".s"
      val exeFile = "program"
      val expectedOutputFile = fileName.stripSuffix(".wacc") + ".expected"

      val compileCmd = s"./compile $fileName"
      assert(compileCmd.! == 0, s"Compilation failed for $fileName")

      // Ensure assembly file was created
      assert(new File(asmFile).exists(), s"Missing assembly file: $asmFile")

      val assembleCmd = s"aarch64-linux-gnu-gcc -o $exeFile -z noexecstack -march=armv8-a $asmFile"
      assert(assembleCmd.! == 0, s"Assembly failed for $asmFile")

      val actualOutput = s"qemu-aarch64 -L $qemuPath $exeFile".!!
      val expectedOutput = Source.fromFile(expectedOutputFile).getLines().mkString("\n")

      assert(actualOutput.trim == expectedOutput.trim, s"Output mismatch for $fileName")
    }

  validFiles.foreach{ testFile }

  // invalidFiles.foreach{ testFile }
}
