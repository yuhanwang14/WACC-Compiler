import scala.sys.process._

object UpdateTestFile {
  def updateFileLists(): Unit = {
    val updateCmd = "./src/test/wacc/get_files.sh"
    val exitCode = updateCmd.!
    assert(exitCode == 0, "Failed to update test file lists")
    println("Test file lists updated successfully.")
  }
}
