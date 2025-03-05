package testCode

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

  object CompilerRunner {
    /* Runs the `compile` command and captures both exit code and output */
    def compile(fileName: String): (Int, String) = {
      val compileCmd = s"./compile $fileName"
      val output = new StringBuilder
      val exitCode = compileCmd ! ProcessLogger(output.append(_))
      (exitCode, output.toString)
    }
  }

  def testFile(fileName: String, isValid: Boolean): Unit = test(s"${if (isValid) "Valid" else "Invalid"} Backend Test: $fileName") {
    val asmFile = fileName.stripSuffix(".wacc") + ".s"
    val exeFile = "program"
    val expectedOutputFile = fileName.stripSuffix(".wacc") + ".expected"

    // Compile the file
    val (compileExitCode, errorOutput) = CompilerRunner.compile(fileName)

    if (isValid) {
      // Expected behavior for valid files
      assert(compileExitCode == 0, s"Compilation failed for $fileName with exit code $compileExitCode")
      assert(new File(asmFile).exists(), s"Missing assembly file: $asmFile")

      // Assemble and run
      val assembleCmd = s"aarch64-linux-gnu-gcc -o $exeFile -z noexecstack -march=armv8-a $asmFile"
      assert(assembleCmd.! == 0, s"Assembly failed for $asmFile")

      val actualOutput = s"qemu-aarch64 -L $qemuPath $exeFile".!!
      val expectedOutput = Source.fromFile(expectedOutputFile).getLines().mkString("\n")
      assert(actualOutput.trim == expectedOutput.trim, s"Output mismatch for $fileName")

    } else {
      // Expected behavior for invalid files
      assert(
        compileExitCode == 100 || compileExitCode == 200,
        s"Unexpected exit code $compileExitCode for $fileName (Expected 100 or 200)"
      )
      assert(!new File(asmFile).exists(), s"Assembly file $asmFile should NOT be generated for invalid program")
      assert(errorOutput.contains("error"), s"Expected error message in output for $fileName")
    }
  }

  // Run tests for all files
  validFiles.foreach(testFile(_, isValid = true))
  invalidFiles.foreach(testFile(_, isValid = false))
}
