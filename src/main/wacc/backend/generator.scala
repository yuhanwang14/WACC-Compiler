package backend

import ast.*
import common.SymbolTable
import common.types.TypeBridge
import instructions.*
import instructions.Register.*
import scala.collection.mutable.ListBuffer
import common.Scope

object Generator {

  private val localLabelCount: Int = 0

  def generate(prog: Program)(implicit
      symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit = {
    // generateBlock(prog.s)
    prog.fs.foreach(generateFunc(_))
  }

  private def generateBlock(
      block: Stmt, 
      allocator: RegisterAllocator, 
      scope: Scope
  )(implicit
      symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit = {

    val stmts: List[Stmt] = block match {
      case Block(sts) => sts
      case _          => return
    }

    var subScopes = scope.children
    scope.localVars.foreach(x => allocator.allocate(x._1, x._2.byteSize))
    
    for (stmt <- stmts) {
      stmt match {
        case Skip() =>   
        case If(cond, b1, b2) => {
          var newAllocator: RegisterAllocator = allocator.clone()
          generateBlock(b1, newAllocator, subScopes.head)
          subScopes = subScopes.tail
          newAllocator = allocator.clone()
          generateBlock(b2, newAllocator, subScopes.head)
          subScopes = subScopes.tail
        }
        case _ =>
      }
    }

  }

  private def generateFunc(func: Func)(implicit
      symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit = {

    val funcName: String = func.ti._2.name
    symbolTable.enterFunctionScope(funcName)
    asmLine += s"wacc_${funcName}:"

    asmLine += STP(FP, LR, SP, ImmVal(-16), PreIndex).toString()

    // temporarily push all registers saved by Callee
    val numOfVariables = 10
    val (pushCode, popCode) = pushAndPopCalleeRegisters(numOfVariables)

    asmLine += pushCode
    asmLine += MOVReg(FP, SP).toString()

    // extract all parameters from symbolTable, allocate register or memory
    val params = 
      symbolTable.currentScope.varTable.filter(x => x._1.startsWith(s"_func_${funcName}_params"))
    val numOfParams: Int = params.size
    val allocator: RegisterAllocator = RegisterAllocator(numOfVariables, numOfParams)
    for ((name, paramType) <- params) {
      allocator.addParam(name, TypeBridge.fromAst(paramType).byteSize)
    }

    // the current scope is for parameters
    generateBlock(func.s, allocator, symbolTable.currentScope.children.head)
    
    asmLine += popCode
    asmLine += LDP(FP, LR, SP, ImmVal(16), PostIndex).toString()
  }
  
  /**
   * Generate code to push and pop all registers saved by Callee (x19-x28) that are about to use.
   * Assume that 0 < `numReg` <= 10
   */
  private def pushAndPopCalleeRegisters(numReg: Int = 10): (String, String) = {

    val offset = math.floorDiv(numReg + 1, 2) * 16

    if (numReg == 1) {
      val push = STP(XRegister(19), XZR, SP, ImmVal(-offset), PreIndex).toString()
      val pop  = LDP(XRegister(19), XZR, SP, ImmVal(offset), PostIndex).toString()
      (push, pop)
    } else {

      val firstPush = STP(XRegister(19), XRegister(20), SP, ImmVal(-offset), PreIndex).toString()
      val lastPop   = LDP(XRegister(19), XRegister(20), SP, ImmVal(offset), PostIndex).toString()

      val pairedInstrs = (2 until numReg by 2).map { pushedNum =>
        val r1 = XRegister(19 + pushedNum)
        val r2 = if (pushedNum + 1 < numReg) XRegister(20 + pushedNum) else XZR
        (
          STP(r1, r2, SP, ImmVal(8 * pushedNum), Offset).toString(),
          LDP(r1, r2, SP, ImmVal(8 * pushedNum), Offset).toString()
        )
      }
      val pushCode = (firstPush +: pairedInstrs.map(_._1)).mkString("\n")
      val popCode  = (pairedInstrs.map(_._2) :+ lastPop).mkString("\n")
      (pushCode, popCode)
    }
  }

}
