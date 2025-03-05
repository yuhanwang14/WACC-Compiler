package testCode

import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process._
import java.io.File
import scala.io.Source
import scala.util.Using
import wacc.backend.BackendCompiler  
class BackendSpec extends AnyFunSuite {

  // Update test lists before running
  TestFiles.updateFileLists()

  val validFiles = TestFiles.validFiles
  val invalidFiles = TestFiles.invalidFiles

  val qemuPath = "/usr/aarch64-linux-gnu/"

  def testFile(fileName: String, isValid: Boolean): Unit = test(s"${if (isValid) "Valid" else "Invalid"} Backend Test: $fileName") {
    val asmFile = fileName.stripSuffix(".wacc") + ".s"
    val exeFile = "program"
    val expectedOutputFile = fileName.stripSuffix(".wacc") + ".expected"

    val compileExitCode = BackendCompiler.compile(fileName)

    if (isValid) {
      // For valid files, expect successful compilation
      assert(compileExitCode == 0, s"Compilation failed for $fileName with exit code $compileExitCode")
      // The assembly file should be generated
      assert(new File(asmFile).exists(), s"Missing assembly file: $asmFile")

      // Assemble the generated assembly file
      val assembleCmd = s"aarch64-linux-gnu-gcc -o $exeFile -z noexecstack -march=armv8-a $asmFile"
      assert(assembleCmd.! == 0, s"Assembly failed for $asmFile")

      // Run the executable in QEMU and capture its output
      val actualOutput = s"qemu-aarch64 -L $qemuPath $exeFile".!!
      
      // Read expected output using Using for resource management
      val expectedOutput = Using(Source.fromFile(expectedOutputFile)) { source =>
        source.getLines().mkString("\n")
      }.getOrElse("")

      assert(actualOutput.trim == expectedOutput.trim, s"Output mismatch for $fileName")
    } else {
      // For invalid files, expect exit code 100 or 200 and no assembly file
      assert(
        compileExitCode == 100 || compileExitCode == 200,
        s"Unexpected exit code $compileExitCode for $fileName (Expected 100 or 200)"
      )
      assert(!new File(asmFile).exists(), s"Assembly file $asmFile should NOT be generated for invalid program")
    }
  }

  /* Run tests for all valid and invalid files */
  
  // validFiles.foreach(testFile(_, isValid = true))
  invalidFiles.foreach(testFile(_, isValid = false))
}
