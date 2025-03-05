// package testCode

// import org.scalatest.funsuite.AnyFunSuite
// import scala.sys.process._

// class FullPipelineSpec extends AnyFunSuite {

//   // Update test lists before running
//   TestFiles.updateFileLists()

//   val validFiles = TestFiles.validFiles
//   val invalidFiles = TestFiles.validFiles

//   test("Run all valid and invalid WACC programs") {
//     validFiles.foreach { file =>
//       assert(s"./compile $file".! == 0, s"Compilation failed for $file")
//     }

//     invalidFiles.foreach { file =>
//       val exitCode = s"./compile $file".!
//       assert(exitCode == 100 || exitCode == 200, s"Invalid file passed compilation: $file")
//     }
//   }
// }
