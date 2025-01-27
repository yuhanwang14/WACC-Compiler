package wacc

import parser.parse
import parsley.{Success, Failure}
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

class ProgSpec extends UnitSpec {

    val fileList = Files.readAllLines(
        Paths.get("./src/test/wacc/file_lists/prog_files")).asScala

    fileList.foreach { filePath =>
        test(filePath) {
            val sourceCode = Files.readString(Paths.get(filePath))
            parse(sourceCode) match {
                case Failure(msg) => assert(false, msg)
                case Success(_) => assert(true)
            }
        }
    }
}
