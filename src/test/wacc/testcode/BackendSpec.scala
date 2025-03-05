package testcode

import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process._
import java.io.File
import scala.io.Source
import scala.util.Using
import wacc.backend.BackendCompiler  
import common.FileUtil
import TestConfig._


class BackendSpec extends AnyFunSuite {

  // Update test lists before running
  TestFiles.updateFileLists()

  val validFiles = TestFiles.validFiles
  val invalidFiles = TestFiles.invalidFiles

  def testFile(fileName: String, isValid: Boolean): Unit = test(s"${if (isValid) "Valid" else "Invalid"} Backend Test: $fileName") {
    val asmFile = fileName.stripSuffix(".wacc") + ".s"
    val exeFile = "program"
    val expectedOutputFile = fileName.stripSuffix(".wacc") + ".expected"

    // Use the internal compiler function to compile the file.
    val compileExitCode = BackendCompiler.compile(fileName)

    if (isValid) {
      try {
        // For valid files, expect successful compilation
        assert(compileExitCode == 0, s"Compilation failed for $fileName with exit code $compileExitCode")
        
        // Write the assembly file using our utility
        FileUtil.writeFile(asmFile, BackendCompiler.outputString) match {
          case scala.util.Success(_)  => // File written successfully.
          case scala.util.Failure(ex) =>
            fail(s"Failed to write assembly file: ${ex.getMessage}")
        }

        // The assembly file should be generated
        assert(new File(asmFile).exists(), s"Missing assembly file: $asmFile")

        // Assemble the generated assembly file using the selected assembler command
        val assembleCmd = s"$assemblerCmd -o $exeFile -z noexecstack -march=armv8-a $asmFile"
        assert(assembleCmd.! == 0, s"Assembly failed for $asmFile")

        // Run the executable in the selected emulator and capture its output
        val actualOutput = s"$emulatorCmd -L $emulatorLibPath $exeFile".!!
        val expectedOutput = Using(Source.fromFile(expectedOutputFile)) { source =>
          source.getLines().mkString("\n")
        }.getOrElse("")

        assert(actualOutput.trim == expectedOutput.trim, s"Output mismatch for $fileName")
      } finally {
        // Clean up: delete the assembly file and executable
        if (!localMode) {
          FileUtil.deleteFile(asmFile)
          FileUtil.deleteFile(exeFile)
        }
      }
    
    } else {
      // For invalid files, expect exit code 100 or 200 and no assembly file generated.
      assert(
        compileExitCode == 100 || compileExitCode == 200,
        s"Unexpected exit code $compileExitCode for $fileName (Expected 100 or 200)"
      )
      assert(!new File(asmFile).exists(), s"Assembly file $asmFile should NOT be generated for invalid program")
    }
  }

  // Run tests for all valid and invalid files
  // validFiles.foreach(testFile(_, isValid = true))

  invalidFiles.foreach(testFile(_, isValid = false))
}

