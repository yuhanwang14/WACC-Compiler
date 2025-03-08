package testcode

import scala.sys.process._
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import java.io.File

object TestFiles {

  val dir = "./src/test/wacc"

  val validFiles = getFiles("valid_files")
  val invalidFiles = getFiles("invalid_files")

  def updateFileLists(): Unit = {
    val updateCmd = s"$dir/get_files.sh"
    val exitCode = updateCmd.!
    assert(exitCode == 0, "Failed to update test file lists")
    println("Test file lists updated successfully.")
  }

  def getFiles(name: String): Seq[String] = {
    val files = Files.readAllLines(Paths.get(s"$dir/file_lists/$name")).asScala.toSeq

    // Filter out files from unwanted directories and ensure they have a corresponding .expected file
    files.filter { file =>
      val expectedFile = new File(file.stripSuffix(".wacc") + ".expected")
      !file.contains("advanced/") && !file.contains("runtimeErr/") && expectedFile.exists() &&
      !file.contains("IO/") && !file.contains("runtimeErr/") && !file.contains("exit/")
    }
  }
}
