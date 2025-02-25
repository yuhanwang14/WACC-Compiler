package backend

import ast.*
import scala.collection.mutable.ListBuffer

object Generator {

  def generate(prog: Program)(implicit
      // symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit = {
    generateBlock(prog.s)
    prog.fs.foreach(generateFunc(_))
  }

  private def generateBlock(block: Stmt)(implicit
      // symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit = {

    val stmts: List[Stmt] = block match {
      case Block(sts) => sts
      case _          => return
    }
    stmts.foreach(generateStmt(_))

  }

  private def generateStmt(stmt: Stmt)(implicit
      // symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit =  {
    // case Skip() => {
    //   asmLine += DataHeader().toString()
    //   asmLine += STP(FP, LR, SP, new ImmVal(-16), PreIndex).toString()
    //   asmLine += MOVReg(FP, SP).toString()
    //   asmLine += MOVImm(XRegister(0), new ImmVal(0)).toString()
    //   asmLine += LDP(FP, LR, SP, new ImmVal(16), PostIndex).toString()
    //   asmLine += RET.toString()
    // }
    // case _ => 
  }

  private def generateFunc(func: Func)(implicit
      // symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit = {}

}
