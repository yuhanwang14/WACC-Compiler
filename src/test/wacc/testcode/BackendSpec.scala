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

    // Compile the file and capture the result
    val compileResult = BackendCompiler.compile(fileName)

    if (isValid) {
      compileResult match {
        case Right(asmString) =>
          // Write the assembly file using our utility
          FileUtil.writeFile(asmFile, asmString) match {
            case scala.util.Success(_) =>
            case scala.util.Failure(ex) =>
              fail(s"Failed to write assembly file: ${ex.getMessage}")
          }

          // The assembly file should be generated
          assert(new File(asmFile).exists(), s"Missing assembly file: $asmFile")

          // Assemble the generated assembly file using the selected assembler command
          // val assembleCmd = s"clang -target aarch64-linux-gnu -o $exeFile $asmFile"
          val assembleCmd = s"$assemblerCmd -o $exeFile -z noexecstack -march=armv8-a $asmFile"
          

          if (assembleCmd.! != 0) {
            if (!localMode)
              FileUtil.deleteFile(asmFile)
              FileUtil.deleteFile(exeFile)
            assert(false, s"Assembly failed for $asmFile")
          }
          

          // Run the executable in the selected emulator and capture its output
          val actualOutput = s"$emulatorCmd -L $emulatorLibPath $exeFile".!!
          val expectedOutput = Using(Source.fromFile(expectedOutputFile)) { source =>
            source.getLines().mkString("\n")
          }.getOrElse("")

          assert(actualOutput.trim == expectedOutput.trim, s"Output mismatch for $fileName")

        case Left(exitCode) =>
          fail(s"Expected valid compilation, but got exit code $exitCode for $fileName")
      }

      // Clean up generated files only in lab mode
      if (!localMode) {
        FileUtil.deleteFile(asmFile)
        FileUtil.deleteFile(exeFile)
      }
    
    } else {
      // For invalid files, we expect a failure exit code.
      compileResult match {
        case Left(exitCode) =>
          assert(exitCode == 100 || exitCode == 200,
            s"Unexpected exit code $exitCode for $fileName (Expected 100 or 200)")
          assert(!new File(asmFile).exists(), s"Assembly file $asmFile should NOT be generated for invalid program")
        case Right(_) =>
          fail(s"Expected invalid compilation for $fileName, but compilation succeeded.")
      }
    }
  }

  // Run tests for all valid and invalid files
  if (testValid) 
    validFiles.foreach(testFile(_, isValid = true))
  if (testInvalid)
    invalidFiles.foreach(testFile(_, isValid = false))
}
