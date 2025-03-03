package backend

import ast.*
import common.SymbolTable
import common.types.TypeBridge
import instructions.*
import instructions.AsmLabeling.*
import scala.collection.mutable.ListBuffer
import common.Scope
import scala.collection.mutable.ArrayBuffer
import scala.math

object Generator {

  private var localLabelCount: Int = 0

  def generate(prog: Program)(implicit
      symbolTable: SymbolTable,
  ): AsmSnippet = {
    // generateBlock(prog.s)
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    prog.fs.foreach(x => asmLines += generateFunc(x))
    AsmFunction(asmLines.to(Seq)*)
  }

  private def generateBlock(
      block: Stmt,
      allocator: RegisterAllocator,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable,
  ): AsmSnippet = {

    val stmts: List[Stmt] = block match {
      case Block(sts) => sts
      case _          => List()
    }

    var subScopes = scope.children

    // calculate the extra stack space needed for local variables within current scope
    // allocate register or stack space for locak variables within currentScope
    val offsetBefore: Int = math.floorDiv(allocator.varOffset, 16) * 16
    scope.localVars.foreach(x => allocator.allocate(x._1, x._2.byteSize))
    val offsetAfter: Int = math.floorDiv(allocator.varOffset, 16) * 16
    val extraStackSpace: Int = offsetBefore - offsetAfter

    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()

    if (extraStackSpace > 0)
      asmLines += SUBS(sp, sp, ImmVal(extraStackSpace))

    // TODO: implement all statement cases
    for (stmt <- stmts) {
      stmt match {
        case Skip() =>

        case If(cond, b1, b2) => {
          val thenLabel = asmLocal ~ localLabelCount
          val afterLabel = asmLocal ~ (localLabelCount + 1)
          var newAllocator: RegisterAllocator = allocator.clone()

          localLabelCount += 2
          asmLines += Comment(s"TODO: evaluate $cond and bcond to $thenLabel")(4)

          // generate `else` block
          asmLines += generateBlock(b2, newAllocator, subScopes.head)
          subScopes = subScopes.tail
          asmLines += B(afterLabel)

          // generate `then` block
          newAllocator = allocator.clone()
          asmLines += AsmSnippet(thenLabel)(0)
          asmLines += generateBlock(b1, newAllocator, subScopes.head)
          subScopes = subScopes.tail

          asmLines += AsmSnippet(afterLabel)(0)
        }

        case While(cond, block) => {
          val afterLabel = asmLocal ~ localLabelCount
          val loopLabel = asmLocal ~ (localLabelCount + 1)
          val newAllocator: RegisterAllocator = allocator.clone()
          localLabelCount += 2

          asmLines += B(loopLabel)
          asmLines += AsmSnippet(loopLabel)(0)
          asmLines += generateBlock(block, newAllocator, subScopes.head)
          subScopes = subScopes.tail

          asmLines += AsmSnippet(afterLabel)(0)
          asmLines += Comment(s"TODO: evaluate $cond and bcond to $loopLabel")(4)
        }

        case Begin(block) => {
          val newAllocator: RegisterAllocator = allocator.clone()
          asmLines += generateBlock(block, newAllocator, subScopes.head)
          subScopes = subScopes.tail
        }

        case _ =>
      }
    }

    if (extraStackSpace > 0)
      asmLines += ADDS(sp, sp, ImmVal(extraStackSpace))

    AsmFunction(asmLines.to(Seq)*)
  }

  private def generateFunc(func: Func)(implicit
      symbolTable: SymbolTable,
  ): AsmSnippet = {

    val funcName: String = func.ti._2.name
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    symbolTable.enterFunctionScope(funcName)

    // temporarily push all registers saved by Callee
    val numOfVariables = 10
    val calleeRegisters: ArrayBuffer[Register] = (19 to 28).map(n => XRegister(n)).to(ArrayBuffer)
    val (pushCode, popCode) = pushAndPopRegisters(calleeRegisters)

    // extract all parameters from symbolTable, allocate register or memory
    // a naive way of retrieving all parameters, can be modified
    val params =
      func.ps.fold(List())(x => x.ps).map(x => (s"_func_${funcName}_params::" + x.i.name, x.t))
    val numOfParams: Int = params.size
    val allocator: RegisterAllocator = RegisterAllocator(numOfVariables, numOfParams)
    for ((name, paramType) <- params) {
      allocator.addParam(name, TypeBridge.fromAst(paramType).byteSize)
    }

    asmLines += AsmFunction(
      AsmSnippet(f"wacc_${funcName}:")(0),
      Comment("push {fp, lr}")(4),
      STP(fp, lr, PreIndex(sp, ImmVal(-16))),
      pushCode,
      MOV(fp, sp),
      // the current scope is for parameters
      generateBlock(func.s, allocator, symbolTable.currentScope.children.head),
      popCode,
      Comment("pop {fp, lr}")(4),
      LDP(fp, lr, PreIndex(sp, ImmVal(-16))),
      RET
    )
    AsmFunction(asmLines.to(Seq)*)
  }

  /** Generate assembly code to evaluate the result of a rvalue.
    */
  private def generateRValue(
      rvalue: RValue,
      allocator: RegisterAllocator,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    // wip
    rvalue match

      case Call(Ident(funcName), ArgList(argList)) => {
        val (pushCode, popCode) = pushAndPopRegisters(allocator.callerRegister)
        asmLines += pushCode
        val (argPushCode, offset) = pushArgs(funcName, argList, allocator, scope)
        if (offset > 0)
          asmLines += SUBS(sp, sp, ImmVal(offset))
        asmLines += argPushCode
        asmLines += BL(asmGlobal ~ f"wacc_$funcName")
        asmLines += MOV(XRegister(8), XRegister(0))

        if (offset > 0)
          asmLines += ADDS(sp, sp, ImmVal(offset))
        asmLines += popCode
      }

      case _ =>

    AsmFunction(asmLines.to(Seq)*)
  }

  /** Generate assembly code to evaluate the result of an expression The default position of result
    * is register X8
    */
  private def generateExpr(
      expr: Expr,
      allocator: RegisterAllocator,
      scope: Scope,
      dest: Register = XRegister(8)
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    // TODO
    Comment(s"TODO: evaluate $expr and move to $dest")(4)
  }

  /** Generate assemply code to calculate a list of expr and push them into the stack. Return a list
    * of code string and an offset indicating the amount of stack space needed
    */
  private def pushArgs(
      funcName: String,
      argList: List[Expr],
      allocator: RegisterAllocator,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): (AsmSnippet, Int) = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val paramTypes = symbolTable.lookupFunction(funcName).fold(Nil)(x => x.paramTypes)
    var paramCount: Int = 0
    var offset: Int = 0

    for ((expr, paramType) <- argList.zip(paramTypes)) {
      val paramSize: Int = TypeBridge.fromAst(paramType).byteSize
      val dest = if paramSize > 4 then XRegister(paramCount) else WRegister(paramCount)
      if (paramCount < 8) {
        asmLines += generateExpr(expr, allocator, scope, dest)
      } else {
        asmLines += generateExpr(expr, allocator = allocator, scope = scope)
        asmLines += STR(dest, Offset(sp, ImmVal(offset)))
        offset += paramSize
        paramCount -= 1
      }
      paramCount += 1
    }
    (AsmFunction(asmLines.to(Seq)*), math.floorDiv(offset + 15, 16) * 16)
    
  }

  /** Generate code to push and pop all registers in `regs` to the stack. The generated code works
    * if only if the stack pointers (sp) after the push and before the pop are the same.
    */
  private def pushAndPopRegisters(
      regs: ArrayBuffer[Register]
  ): (AsmSnippet, AsmSnippet) = {

    val numReg: Int = regs.size
    val offset = (numReg + 1) / 2 * 16

    if (numReg == 0)
      return (AsmSnippet("")(0), AsmSnippet("")(0))

    val pushComment = Comment(s"push {${regs.mkString(", ")}}")(4)
    val popComment = Comment(s"pop {${regs.mkString(", ")}}")(4)
    if (numReg == 1) {
      val pushCode = STP(regs(0), xzr, PreIndex(sp, ImmVal(-offset)))
      val popCode = LDP(regs(0), xzr, PostIndex(sp, ImmVal(offset)))
      (pushCode, popCode)
    } else {

      val firstPush = STP(regs(0), regs(1), PreIndex(sp, ImmVal(-offset)))
      val lastPop = LDP(regs(0), regs(1), PostIndex(sp, ImmVal(offset)))

      val pairedInstrs = (2 until numReg by 2).map { pushedNum =>
        val r1 = regs(pushedNum)
        val r2 = if (pushedNum + 1 < numReg) regs(pushedNum + 1) else xzr
        (
          STP(r1, r2, Offset(sp, ImmVal(8 * pushedNum))),
          LDP(r1, r2, Offset(sp, ImmVal(8 * pushedNum)))
        )
      }
      val pushCode = (firstPush +: pairedInstrs.map(_._1))
      val popCode = (pairedInstrs.map(_._2) :+ lastPop)
      (
        AsmFunction((pushComment +: pushCode)*), 
        AsmFunction((popComment +: popCode)*)
      )
    }
  }

}
