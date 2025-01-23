// package wacc

// import parser.parse
// import parsley.{Success, Failure}
// import java.nio.file.{Files, Paths}
// import scala.jdk.CollectionConverters._

// class ExprSpec extends UnitSpec {

//     val fileList = Files.readAllLines(
//         Paths.get("./src/test/wacc/file_lists/expr_files")).asScala
    
//     fileList.foreach { filePath =>
//         test(filePath) {
//             val sourceCode = Files.readString(Paths.get(filePath))
//             parse(sourceCode) match {
//                 case Failure(msg) => assert(false, msg)
//                 case Success(_) => assert(true)
//             }
//         }
//     }
// }


// class TypeSpec extends UnitSpec {

//     val fileList = Files.readAllLines(
//         Paths.get("./src/test/wacc/file_lists/type_files")).asScala

//     fileList.foreach { filePath =>
//         test(filePath) {
//             val sourceCode = Files.readString(Paths.get(filePath))
//             parse(sourceCode) match {
//                 case Failure(msg) => assert(false, msg)
//                 case Success(_) => assert(true)
//             }
//         }
//     }
// }
