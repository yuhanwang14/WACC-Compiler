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
          // Write the assembly file
          FileUtil.writeFile(asmFile, asmString) match {
            case scala.util.Success(_) =>
            case scala.util.Failure(ex) =>
              fail(s"Failed to write assembly file: ${ex.getMessage}")
          }

          assert(new File(asmFile).exists(), s"Missing assembly file: $asmFile")

          // Run everything inside Docker to avoid macOS issues
          val dockerCommand = Seq(
            "docker", "run", "--rm",
            "-v", s"${System.getProperty("user.dir")}:/workspace",
            "wacc-tester",
            "sh", "-c",
            s"$assemblerCmd -o /workspace/$exeFile -z noexecstack -march=armv8-a /workspace/$asmFile && " +
            s"$emulatorCmd -L $emulatorLibPath /workspace/$exeFile"
          )

          val actualOutput = dockerCommand.!!
          val expectedOutput = Using(Source.fromFile(expectedOutputFile)) { source =>
            source.getLines().mkString("\n")
          }.getOrElse("")

          assert(actualOutput.trim == expectedOutput.trim, s"Output mismatch for $fileName")

        case Left(exitCode) =>
          fail(s"Expected valid compilation, but got exit code $exitCode for $fileName")
      }

      // Cleanup files
      FileUtil.deleteFile(asmFile)
      FileUtil.deleteFile(exeFile)

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
