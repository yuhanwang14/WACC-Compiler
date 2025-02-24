package wacc

import frontend.parser.*
import semanticCheckers.*
import java.nio.file.{Files, Paths}
import java.io.File
import scala.jdk.CollectionConverters.*
import scala.util.Success
import scala.util.Failure
import scala.io.Source
import scala.collection.mutable.ListBuffer

class ValidSpec extends UnitSpec {

  val fileList = Files
    .readAllLines(Paths.get("./src/test/wacc/file_lists/valid_files"))
    .asScala

  fileList.foreach { fileName =>
    test(fileName) {
      val src: File = new File(fileName)

      parse(src) match
        case Success(result) =>
          result match
            case parsley.Success(prog) =>
              implicit val lines: Seq[String] =
                Source.fromFile(src).getLines().toSeq
              implicit val sourceName: String =
                fileName
              ProgramChecker.check(prog) match
                case ListBuffer() => {
                  println(prog)
                  assert(true)
                }
                case errors =>
                  println("#semantic_error#")
                  errors.map { error =>
                    println(error.format)
                  }
                  assert(false)

            case parsley.Failure(error) =>
              println("#syntax_error#")
              println(error.format)
              assert(false)

        case Failure(_) =>
          println(s"Can't find or read source file at $fileName")
          assert(false)
    }
  }
}
