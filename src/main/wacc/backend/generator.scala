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
    // allocate register or stack space for locak variables within currentScope
    val offsetBefore: Int = math.floorDiv(allocator.varOffset, 16) * 16
    scope.localVars.foreach(x => allocator.allocate(x._1, x._2.byteSize))
    val offsetAfter: Int = math.floorDiv(allocator.varOffset, 16) * 16
    val extraStackSpace: Int = offsetBefore - offsetAfter

    if (extraStackSpace > 0)
      asmLine += SUBS(SP, SP, ImmVal(extraStackSpace)).toString
    
    // wip
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
          asmLine += thenLabel.toString
          generateBlock(b1, newAllocator, subScopes.head)
          subScopes = subScopes.tail

          asmLine += afterLabel.toString

        }
        case Declare(ti, rvalue) => {
          val name: String = ti._2.name
          scope.shadow(name)
          asmLine ++= generateRValue(rvalue, allocator, scope)
        }
        case _ =>
      }
    }

    if (extraStackSpace > 0)
      asmLine += ADDS(SP, SP, ImmVal(extraStackSpace)).toString

  }

  private def generateFunc(func: Func)(implicit
      symbolTable: SymbolTable,
      asmLine: ListBuffer[String]
  ): Unit = {

    val funcName: String = func.ti._2.name
    symbolTable.enterFunctionScope(funcName)
    asmLine += s"wacc_${funcName}:"

    asmLine += Comment("push {fp, lr}")(4).toString
    asmLine += STP(FP, LR, SP, ImmVal(-16), PreIndex).toString

    // temporarily push all registers saved by Callee
    val numOfVariables = 10
    val calleeRegisters: ArrayBuffer[Register] = (19 to 28).map(n => XRegister(n)).to(ArrayBuffer)
    val (pushCode, popCode) = pushAndPopRegisters(calleeRegisters)

    asmLine ++= pushCode
    asmLine += MOVReg(FP, SP).toString

    // extract all parameters from symbolTable, allocate register or memory
    // a naive way of retrieving all parameters, can be modified
    val params = 
      func.ps.fold(List())(x => x.ps).map(x => (s"_func_${funcName}_params::" + x.i.name, x.t))
    val numOfParams: Int = params.size
    val allocator: RegisterAllocator = RegisterAllocator(numOfVariables, numOfParams)
    for ((name, paramType) <- params) {
      allocator.addParam(name, TypeBridge.fromAst(paramType).byteSize)
    }

    // the current scope is for parameters
    generateBlock(func.s, allocator, symbolTable.currentScope.children.head)
    
    // pop saved registers
    asmLine ++= popCode
    asmLine += Comment("pop {fp, lr}")(4).toString
    asmLine += LDP(FP, LR, SP, ImmVal(16), PostIndex).toString
    asmLine += RET.toString
  }

  private def generateRValue(rvalue: RValue, allocator: RegisterAllocator, scope: Scope)(implicit
    symbolTable: SymbolTable,
  ): ListBuffer[String] = {
    val codeLines: ListBuffer[String] = ListBuffer()
    // wip
    rvalue match
      case Call(Ident(funcName), ArgList(argList)) => {
        val (pushCode, popCode) = pushAndPopRegisters(allocator.callerRegister)
        codeLines ++= pushCode
        val (argPushCode, offset) = pushArgs(funcName, argList, allocator, scope)
        if (offset > 0)
          codeLines += SUBS(SP, SP, ImmVal(offset)).toString
        codeLines ++= argPushCode
        codeLines += BL(GlobalLabel(f"wacc_$funcName")).toString
        if (offset > 0)
          codeLines += ADDS(SP, SP, ImmVal(offset)).toString
        codeLines ++= popCode
      }
      case _  =>
    codeLines
  }

  /**
   * Generate assembly code to calculate the expression `expr`.
   * The default position of result is register X8
   */
  private def generateExpr(expr: Expr, 
      dest: Register = XRegister(8), 
      allocator: RegisterAllocator,
      scope: Scope)
  (implicit
      symbolTable: SymbolTable,
  ): ListBuffer[String] = {
    // wip
    ListBuffer()
  }

  private def pushArgs(
      funcName: String, 
      argList: List[Expr],
      allocator: RegisterAllocator,
      scope: Scope)
  (implicit
      symbolTable: SymbolTable
  ): (ListBuffer[String], Int) = {
    val codeLines: ListBuffer[String] = ListBuffer()
    val paramTypes = symbolTable.lookupFunction(funcName).fold(Nil)(x => x.paramTypes)
    var paramCount: Int = 0
    var offset: Int = 0

    for((expr, paramType) <- argList.zip(paramTypes)) {
      val paramSize: Int = TypeBridge.fromAst(paramType).byteSize
      val dest = if paramSize > 4 then XRegister(paramCount) else WRegister(paramCount)
      if (paramCount < 8) {
        codeLines ++= generateExpr(expr, dest, allocator, scope)
      } else {
        codeLines ++= generateExpr(expr, allocator = allocator, scope = scope)
        codeLines += STR(dest, SP, ImmVal(offset), Offset).toString
        offset += paramSize
        paramCount -= 1
      }
      paramCount += 1
    }
    (codeLines, math.floorDiv(offset + 15, 16) * 16)
  }

  /**
   * Generate code to push and pop all registers in `regs` to the stack.
   * The generated code works if only if the stack pointers (sp) after the push and before 
   * the pop are the same. 
   */
  private def pushAndPopRegisters(
      regs: ArrayBuffer[Register]
  ): (ListBuffer[String], ListBuffer[String]) = {

    val numReg: Int = regs.size
    val offset = (numReg + 1) / 2 * 16
    
    if (numReg == 0)
      return (ListBuffer(), ListBuffer())
    
    val pushComment = Comment(s"push {${regs.mkString(", ")}}")(4).toString
    val popComment = Comment(s"pop {${regs.mkString(", ")}}")(4).toString
    if (numReg == 1) {
      val pushCode = STP(regs(0), XZR, SP, ImmVal(-offset), PreIndex).toString
      val popCode  = LDP(regs(0), XZR, SP, ImmVal(offset), PostIndex).toString
      (ListBuffer(pushCode), ListBuffer(popCode))
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
      val pushCode = (firstPush +: pairedInstrs.map(_._1)).to(ListBuffer)
      val popCode  = (pairedInstrs.map(_._2) :+ lastPop).to(ListBuffer)
      (pushComment +: pushCode.map(_.toString), popComment +: popCode.map(_.toString))
    }
  }

}
