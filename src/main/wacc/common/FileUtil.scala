package common

import java.io.{File, PrintWriter}
import scala.util.{Using, Try}

object FileUtil {
  /** Writes content to a file.
    * @param fileName the file path
    * @param content the content to write
    * @return a Try[Unit] that is a Success if writing succeeded or a Failure otherwise.
    */
  def writeFile(fileName: String, content: String): Try[Unit] = {
    Using(new PrintWriter(new File(fileName))) { writer =>
      writer.write(content)
    }
  }

  /** Deletes the file at the given path.
    * @param fileName the file path
    * @return a Try[Unit] that is Success if deletion succeeded or Failure otherwise.
    */
  def deleteFile(fileName: String): Try[Unit] = {
    val file = new File(fileName)
    if (file.exists()) {
      if (file.delete()) {
        scala.util.Success(())
      } else {
        scala.util.Failure(new Exception(s"Failed to delete file: $fileName"))
      }
    } else {
      scala.util.Success(())
    }
  }
}
