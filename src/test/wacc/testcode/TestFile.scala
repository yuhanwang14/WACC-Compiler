package testcode

import scala.sys.process._
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

object TestFiles {

  val dir = "./src/test/wacc"
  val validFiles = getFiles("valid_if")
  val invalidFiles = getFiles("invalid_files")

  def updateFileLists(): Unit = {
    val updateCmd = s"$dir/get_files.sh"
    val exitCode = updateCmd.!
    assert(exitCode == 0, "Failed to update test file lists")
    println("Test file lists updated successfully.")
  }

  def getFiles(name: String): Seq[String] = {
    Files.readAllLines(Paths.get(s"$dir/file_lists/$name")).asScala.toSeq
  }
}
