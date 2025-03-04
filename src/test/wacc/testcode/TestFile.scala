package testcode

import scala.sys.process._
import java.nio.file.{Files, Paths}

object TestFiles {
  val dir = "./src/test/wacc"
  val validFiles = Files.readAllLines(Paths.get(s"$dir/file_lists/valid_files")).asScala.toSeq
  val invalidFiles = Files.readAllLines(Paths.get(s"$dir/file_lists/invalid_files")).asScala.toSeq

  def updateFileLists(): Unit = {
    val updateCmd = s"$dir/get_files.sh"
    val exitCode = updateCmd.!
    assert(exitCode == 0, "Failed to update test file lists")
    println("Test file lists updated successfully.")
  }
}
