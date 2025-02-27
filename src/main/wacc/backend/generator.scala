package backend

import ast.*
import common.SymbolTable
import backend.RegisterAllocator
import instructions.*
import instructions.Register.*
import scala.collection.mutable.ListBuffer
import math.floorDiv

object Generator {

  def generate(prog: Program)(implicit
      symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit = {
    // generateBlock(prog.s)
    prog.fs.foreach(generateFunc(_))
  }

  private def generateBlock(block: Stmt, allocator: RegisterAllocator)(implicit
      // symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit = {

    val stmts: List[Stmt] = block match {
      case Block(sts) => sts
      case _          => return
    }
    stmts.foreach(generateStmt(_, allocator))

  }

  private def generateStmt(stmt: Stmt, allocator: RegisterAllocator)(implicit
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
  ): Unit = {

    asmLine += s"wacc_${func.ti._2.name}:"

    asmLine += STP(FP, LR, SP, ImmVal(-16), PreIndex).toString()
    // temporarily push all registers saved by Callee
    asmLine += pushCalleeRegisters(10)
    asmLine += MOVReg(FP, SP).toString()
    
  }
  
  /**
   * Push all registers saved by Callee (x19-x28) that are about to use.
   * Assume that `numReg` > 0
   */
  private def pushCalleeRegisters(numReg: Int = 10): String = {
      val codeLines: ListBuffer[Instruction] = ListBuffer()
      val offset: Int = floorDiv(numReg + 1, 2) * 16

      if (numReg == 1)
        STP(XRegister(19), XZR, SP, ImmVal(-16), PreIndex).toString()
      else {
        codeLines += STP(XRegister(19), XRegister(20), SP, ImmVal(-offset), PreIndex)
        var pushedNum: Int = 2
        while(pushedNum < numReg) {
          val R1 = XRegister(19 + pushedNum)
          val R2 = if (numReg - pushedNum > 1) then XRegister(20 + pushedNum) else XZR
          codeLines += STP(R1, R2, SP, ImmVal(8 * pushedNum), Offset)
          pushedNum = pushedNum + 2
        }
        codeLines.mkString("\n")
      } 
  }

}
