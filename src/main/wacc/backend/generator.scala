package backend

import ast.*
import common.SymbolTable
import common.types.TypeBridge
import instructions.*
import instructions.Register.*
import scala.collection.mutable.ListBuffer
import common.Scope
import scala.collection.mutable.ArrayBuffer
import scala.math

object Generator {

  private var localLabelCount: Int = 0

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

    // calculate the extra stack space needed for local variables within current scope
    val offsetBefore: Int = math.floorDiv(allocator.varOffset, 16) * 16
    scope.localVars.foreach(x => allocator.allocate(x._1, x._2.byteSize))
    val offsetAfter: Int = math.floorDiv(allocator.varOffset, 16) * 16
    val extraStackSpace: Int = offsetBefore - offsetAfter

    if (extraStackSpace > 0)
      asmLine += SUBS(SP, SP, ImmVal(extraStackSpace)).toString()
    
    for (stmt <- stmts) {
      stmt match {
        case Skip() =>   
        case If(cond, b1, b2) => {

          val thenLabel = LocalLabel(f"${localLabelCount}")
          val afterLabel = LocalLabel(f"${localLabelCount + 1}")
          localLabelCount += 2

          // generate `else`` block
          var newAllocator: RegisterAllocator = allocator.clone()
          generateBlock(b2, newAllocator, subScopes.head)
          subScopes = subScopes.tail

          // generate `then` block
          newAllocator = allocator.clone()
          asmLine += thenLabel.toString()
          generateBlock(b1, newAllocator, subScopes.head)
          subScopes = subScopes.tail

          asmLine += afterLabel.toString()

        }
        case Declare(ti, rvalue) => {
          val name: String = ti._2.name
          scope.shadow(name)
        }
        case _ =>
      }
    }

    if (extraStackSpace > 0)
      asmLine += ADDS(SP, SP, ImmVal(extraStackSpace)).toString()

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
    val calleeRegisters: ArrayBuffer[Register] = (19 to 28).map(n => XRegister(n)).to(ArrayBuffer)
    val (pushCode, popCode) = pushAndPopRegisters(calleeRegisters)

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
    
    // pop saved registers
    asmLine += popCode
    asmLine += LDP(FP, LR, SP, ImmVal(16), PostIndex).toString()
    asmLine += RET.toString()
  }

  /**
   * Generate code to push and pop all registers in `regs` to the stack.
   * Assume that `regs` is non-empty.
   * The generated code works if only if the stack pointers (sp) after the push and before 
   * the pop are the same. 
   */
  private def pushAndPopRegisters(regs: ArrayBuffer[Register]): (String, String) = {

    val numReg: Int = regs.size
    val offset = math.floorDiv(numReg + 1, 2) * 16

    if (numReg == 1) {
      val pushCode = STP(regs(0), XZR, SP, ImmVal(-offset), PreIndex).toString()
      val popCode  = LDP(regs(0), XZR, SP, ImmVal(offset), PostIndex).toString()
      (pushCode, popCode)
    } else {

      val firstPush = STP(regs(0), regs(1), SP, ImmVal(-offset), PreIndex)
      val lastPop   = LDP(regs(0), regs(1), SP, ImmVal(offset), PostIndex)

      val pairedInstrs = (2 until numReg by 2).map { pushedNum =>
        val r1 = regs(pushedNum)
        val r2 = if (pushedNum + 1 < numReg) regs(pushedNum + 1) else XZR
        (
          STP(r1, r2, SP, ImmVal(8 * pushedNum), Offset),
          LDP(r1, r2, SP, ImmVal(8 * pushedNum), Offset)
        )
      }
      val pushCode = (firstPush +: pairedInstrs.map(_._1)).mkString("\n")
      val popCode  = (pairedInstrs.map(_._2) :+ lastPop).mkString("\n")
      (pushCode, popCode)

    }
  }

}
