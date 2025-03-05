package backend

import ast.*
import common.SymbolTable
import common.types.TypeBridge
import instructions.*
import instructions.AsmLabeling.*
import scala.collection.mutable.ListBuffer
import common.Scope
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map as MutableMap
import scala.collection.mutable.Set as MutableSet
import scala.math
import instructions.PredefinedFunctions.*
import instructions.*

object Generator {

  private var localLabelCount: Int = 0
  private val _stringConsts: MutableMap[String, Int] = MutableMap()
  private val _predefinedFuncs: MutableSet[PredefinedFunc] = MutableSet()

  def generate(prog: Program)(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    asmLines += AsmFunction(
      DataHeader(),
      AsmFunction(
        _stringConsts
          .map((str, index) => {
            LabelledStringConst(asmLocal ~ f"str$index", str)
          })
          .to(Seq)*
      ),
      TextHeader(),
      GlobalHeader("main"),
      LabelHeader("main"),
      Comment("push {fp, lr}")(4),
      STP(fp, lr, PreIndex(sp, ImmVal(-16))),
      // pushCode,
      MOV(fp, sp),
      generateBlock(prog.s, RegisterAllocator(), symbolTable.currentScope.children.head),
      // popCode,
      Comment("pop {fp, lr}")(4),
      LDP(fp, lr, PreIndex(sp, ImmVal(-16))),
      RET
    )
    prog.fs.foreach(func => asmLines += generateFunc(func))
    _predefinedFuncs.foreach(name => asmLines += predefinedFunctions(name))
    AsmFunction(asmLines.to(Seq)*)
  }

  private def generateBlock(
      block: Stmt,
      allocator: RegisterAllocator,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
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

        // TODO: implement branching
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
          asmLines += LabelHeader(thenLabel)
          asmLines += generateBlock(b1, newAllocator, subScopes.head)
          subScopes = subScopes.tail

          asmLines += LabelHeader(afterLabel)
        }

        // TODO: implement branching
        case While(cond, block) => {
          val afterLabel = asmLocal ~ localLabelCount
          val loopLabel = asmLocal ~ (localLabelCount + 1)
          val newAllocator: RegisterAllocator = allocator.clone()
          localLabelCount += 2

          asmLines += B(loopLabel)
          asmLines += LabelHeader(loopLabel)
          asmLines += generateBlock(block, newAllocator, subScopes.head)
          subScopes = subScopes.tail

          asmLines += LabelHeader(afterLabel)
          asmLines += Comment(s"TODO: evaluate $cond and bcond to $loopLabel")(4)
        }

        case Begin(block) => {
          val newAllocator: RegisterAllocator = allocator.clone()
          asmLines += generateBlock(block, newAllocator, subScopes.head)
          subScopes = subScopes.tail
        }

        case Exit(expr) => {
          asmLines += generateExpr(expr, allocator, scope, XRegister(0))
          asmLines += BL("exit")
        }

        case Declare(ti, rvalue) => {
          asmLines += generateRValue(rvalue, allocator, scope)
          val name: String = ti._2.name
          val varType = ti._1
          scope.shadow(name)
          val prefixedName = scope.shadower(name).getOrElse("")
          val location = allocator.getLocation(prefixedName)
          location match {
            case Left(reg) => asmLines += MOV(reg, XRegister(8))
            case Right(offset) => {
              varType match {
                case BoolType() | CharType() => STURB(WRegister(8), Offset(fp, ImmVal(offset)))
                case IntType()               => STUR(WRegister(8), Offset(fp, ImmVal(offset)))
                case _                       => STUR(XRegister(8), Offset(fp, ImmVal(offset)))
              }
            }
          }
        }

        // TODO: need to implement generateLValue
        case Assign(lValue, rValue) => ???

        // TODO: need the type of expr to decide the location of result (x0 or x8)
        case Return(expr) => {
          asmLines += generateExpr(expr, allocator, scope)
        }

        // TODO: need the type of expr to decide the function called for print
        case Print(expr) => {
          val (pushCode, popCode) = pushAndPopRegisters(allocator.callerRegister)
          asmLines += pushCode
          asmLines += generateExpr(expr, allocator, scope, XRegister(0))
          // asmLines += ???
          asmLines += popCode
        }

        // TODO: need the type of expr to decide the function called for print
        case Println(expr) => {
          val (pushCode, popCode) = pushAndPopRegisters(allocator.callerRegister)
          asmLines += pushCode
          asmLines += generateExpr(expr, allocator, scope, XRegister(0))
          // asmLines += ???
          asmLines += AsmSnippet("_println")(0)
          asmLines += popCode
        }

        // TODO: need the type of expr to decide the content of freeing
        case Free(expr) => ???

        // TODO: need to implement generateLValue
        case Read(lvalue) => ???

        case _ =>
      }
    }

    if (extraStackSpace > 0)
      asmLines += ADDS(sp, sp, ImmVal(extraStackSpace))

    AsmFunction(asmLines.to(Seq)*)
  }

  private def generateFunc(func: Func)(implicit
      symbolTable: SymbolTable
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
      LabelHeader(f"wacc_$funcName"),
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
    // TODO
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

      // TODO: need the type of expr to infer the size of array
      // Currently assume that the size of element is 4
      case ArrayLiter(exprs) => {
        val (pushCode, popCode) = pushAndPopRegisters(allocator.callerRegister)
        val typeSize = 4
        val arrayLen = exprs.length
        asmLines += AsmFunction(
          pushCode,
          MOV(WRegister(0), ImmVal(4 + typeSize * arrayLen)),
          BL("_malloc"),
          MOV(ip0, XRegister(0)),
          popCode,
          ADDS(ip0, ip0, ImmVal(4)),
          MOV(WRegister(8), ImmVal(arrayLen)),
          STUR(WRegister(8), Offset(ip0, ImmVal(-4)))
        )
        exprs.zipWithIndex.map { (expr, ind) =>
          asmLines += generateExpr(expr, allocator, scope)
          asmLines += STUR(WRegister(8), Offset(ip0, ImmVal(ind * typeSize)))
        }
        asmLines += MOV(XRegister(8), ip0)
      }

      case NewPair(expr1, expr2) => {
        val (pushCode, popCode) = pushAndPopRegisters(allocator.callerRegister)
        asmLines += AsmFunction(
          pushCode,
          MOV(WRegister(0), ImmVal(16)),
          BL("_malloc"),
          MOV(ip0, XRegister(0)),
          popCode,
          generateExpr(expr1, allocator, scope),
          STUR(XRegister(8), Offset(ip0, ImmVal(0))),
          generateExpr(expr2, allocator, scope),
          STUR(XRegister(8), Offset(ip0, ImmVal(8))),
          MOV(XRegister(8), ip0)
        )
      }

      case First(lvalue) => {
        lvalue match {
          case pairElem: PairElem => generateRValue(pairElem, allocator, scope)
          case otherwise: Expr    => generateExpr(otherwise, allocator, scope)
        }
        asmLines += AsmFunction(
          CMP(XRegister(8), ImmVal(0)),
          BCond("_errNull", Cond.EQ),
          MOV(ip0, XRegister(8)),
          LDUR(XRegister(8), Offset(ip0, ImmVal(0)))
        )
      }

      case Second(lValue) => {
        lValue match {
          case pairElem: PairElem => generateRValue(pairElem, allocator, scope)
          case otherwise: Expr    => generateExpr(otherwise, allocator, scope)
        }
        asmLines += AsmFunction(
          CMP(XRegister(8), ImmVal(0)),
          BCond("_errNull", Cond.EQ),
          MOV(ip0, XRegister(8)),
          LDUR(XRegister(8), Offset(ip0, ImmVal(1)))
        )
      }

      case expr: Expr =>

      case _ =>

    AsmFunction(asmLines.to(Seq)*)
  }

  /** Generate assembly code to move the content of x8 to a given location
    */
  private def generateLValue(
      lValue: LValue,
      allocator: RegisterAllocator,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {

    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    lValue match {

      case Ident(name) => {
        val prefixedName = scope.shadower(name).getOrElse("")
        val location = allocator.getLocation(prefixedName)
        val varType = scope.lookupSymbol(prefixedName).getOrElse(anyType)
        location match {
          case Left(reg) => asmLines += MOV(reg, XRegister(8))
          case Right(offset) => {
            varType match {
              case BoolType() | CharType() =>
                asmLines += STURB(WRegister(8), Offset(fp, ImmVal(offset)))
              case _ => asmLines += STUR(XRegister(8), Offset(fp, ImmVal(offset)))
            }
          }
        }
      }

      case ArrayElem(Ident(name), exprs) => ???
      case First(lValue)                 => ???
      case Second(lValue)                => ???
      case _                             =>
    }
    AsmFunction(asmLines.toList*)
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
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()

    expr match {
      case IntLiter(x)  => asmLines += MOV(dest.asW, ImmVal(x))
      case BoolLiter(x) => asmLines += MOV(dest.asW, ImmVal(if (x) then 1 else 0))
      case CharLiter(c) => asmLines += MOV(dest.asW, ImmVal(c))
      case StrLiter(s)  => {
        var index = _stringConsts.size
        if (_stringConsts.contains(s)) {
          index = _stringConsts(s)
        } else {
          _stringConsts(s) = index
        }
        asmLines += ADRP(dest, asmLocal ~ f".str$index")
        asmLines += ADD(dest, dest, Lo12(asmLocal ~ f".str$index"))
      }
      case PairLiter() => asmLines += MOV(dest, ImmVal(0))
      case Ident(name) => {
        allocator.getLocation(scope.shadower(name).getOrElse("")) match
          case Left(reg)     => asmLines += MOV(dest, reg)
          case Right(offset) => asmLines += LDUR(dest, Offset(fp, ImmVal(offset)))
      }
      case ArrayElem(identName, exprs) => ??? // TODO: Array
      case Paren(e)                    => generateExpr(e, allocator, scope, dest)
      case e: UnaryOp  => asmLines += generateUnary(e, allocator, scope, dest)    // Unary Operations
      case e: BinaryOp => asmLines += generateBinary(e, allocator, scope, dest)  // Binary Operations
    }

    AsmFunction(asmLines.toList*)
  }

  private def generateBinary(
    binaryOp: BinaryOp,
    allocator: RegisterAllocator,
    scope: Scope,
    dest: Register = XRegister(8)
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val w9  = WRegister(9)
    val w10 = WRegister(10)
    val w11 = WRegister(11)

    binaryOp match {
      case Or(expr1, expr2) => 
        {
        asmLines += generateExpr(expr2, allocator, scope, dest)
        asmLines += CMP(dest, ImmVal(1))
        val orLabel = asmLocal ~ localLabelCount
        asmLines += BCond(orLabel, Cond.EQ)
        localLabelCount += 1
        asmLines += generateExpr(expr2, allocator, scope, dest)
        asmLines += CMP(dest, ImmVal(1))
        asmLines += LabelHeader(orLabel)
        asmLines += CSET(dest, Cond.EQ)
      }
      case And(expr1, expr2) => {
        asmLines += generateExpr(expr2, allocator, scope, dest)
        asmLines += CMP(dest, ImmVal(1))
        val andLabel = asmLocal ~ localLabelCount
        asmLines += BCond(andLabel, Cond.NE)
        localLabelCount += 1
        asmLines += generateExpr(expr2, allocator, scope, dest)
        asmLines += CMP(dest, ImmVal(1))
        asmLines += LabelHeader(andLabel)
        asmLines += CSET(dest, Cond.EQ)
      }

      case Equal(expr1, expr2)        => generateComp(expr1, expr2, Cond.EQ, allocator, scope, dest)
      case NotEqual(expr1, expr2)     => generateComp(expr1, expr2, Cond.NE, allocator, scope, dest)
      case Less(expr1, expr2)         => generateComp(expr1, expr2, Cond.LT, allocator, scope, dest)
      case LessEqual(expr1, expr2)    => generateComp(expr1, expr2, Cond.LE, allocator, scope, dest)
      case Greater(expr1, expr2)      => generateComp(expr1, expr2, Cond.GT, allocator, scope, dest)
      case GreaterEqual(expr1, expr2) => generateComp(expr1, expr2, Cond.GE, allocator, scope, dest)

      case Add(expr1, expr2) => {
        asmLines += generateExpr(expr1, allocator, scope, w9)
        asmLines += generateExpr(expr2, allocator, scope, w10)
        asmLines += ADDS(dest.asW, w9, w10)
        asmLines += BCond(asmGlobal ~ P_ErrOverflow, Cond.VS)
        _predefinedFuncs += P_ErrOverflow
        _predefinedFuncs += P_Prints
      }
      case Sub(expr1, expr2) => {
        asmLines += generateExpr(expr1, allocator, scope, w9)
        asmLines += generateExpr(expr2, allocator, scope, w10)
        asmLines += SUBS(dest.asW, w9, w10)
        asmLines += BCond(asmGlobal ~ P_ErrOverflow, Cond.VS)
        _predefinedFuncs += P_ErrOverflow
        _predefinedFuncs += P_Prints
      }
      case Mul(expr1, expr2) => {
        asmLines += generateExpr(expr1, allocator, scope, w9)
        asmLines += generateExpr(expr2, allocator, scope, w10)
        asmLines += SMULL(dest, w9, w10)
        asmLines += CMP(dest, dest.asW, Some(Extend.SXTW))
        asmLines += BCond(asmGlobal ~ P_ErrOverflow, Cond.NE)
        _predefinedFuncs += P_ErrOverflow
        _predefinedFuncs += P_Prints
      }
      case Div(expr1, expr2) => {
        asmLines += generateExpr(expr2, allocator, scope, w9)
        asmLines += CMP(w9, ImmVal(0))
        asmLines += BCond(asmGlobal ~ P_ErrDivZero, Cond.EQ)
        _predefinedFuncs += P_ErrDivZero
        _predefinedFuncs += P_Prints
        asmLines += generateExpr(expr1, allocator, scope, w10)
        asmLines += SDIV(dest.asW, w10, w9)
      }
      case Mod(expr1, expr2) => {
        asmLines += generateExpr(expr2, allocator, scope, w9)
        asmLines += CMP(w9, ImmVal(0))
        asmLines += BCond(asmGlobal ~ P_ErrDivZero, Cond.EQ)
        _predefinedFuncs += P_ErrDivZero
        _predefinedFuncs += P_Prints
        asmLines += generateExpr(expr1, allocator, scope, w10)
        asmLines += SDIV(w11, w10, w9)
        asmLines += MSUB(dest.asW, w11, w9, w10)
      }
    }

    AsmFunction(asmLines.toList*)
  }

  private def generateComp(
      expr1: Expr,
      expr2: Expr,
      cond: Cond,
      allocator: RegisterAllocator,
      scope: Scope,
      dest: Register = XRegister(8)
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val w9 = WRegister(9)

    AsmFunction(
      generateExpr(expr1, allocator, scope, w9),
      generateExpr(expr2, allocator, scope, dest.asW),
      CMP(dest.asW, w9),
      CSET(dest.asW, cond)
    )
  }

  private def generateUnary(
      unaryOp: UnaryOp,
      allocator: RegisterAllocator,
      scope: Scope,
      dest: Register = XRegister(8)
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val x1 = XRegister(1)
    val w9 = WRegister(9)

    unaryOp match {
      case Not(e) => {
        asmLines += generateExpr(e, allocator, scope, w9)
        asmLines += CMP(w9, ImmVal(1))
        asmLines += CSET(dest.asW, Cond.NE)
      }
      case Negate(e) => {
        asmLines += generateExpr(e, allocator, scope, w9)
        asmLines += NEGS(dest.asW, w9);
        asmLines += BCond(asmGlobal ~ P_ErrOverflow, Cond.VS)
        _predefinedFuncs += P_ErrOverflow
        _predefinedFuncs += P_Prints
      }
      case Len(e) => ??? // TODO: Array
      case Ord(e) => {
        asmLines += generateExpr(e, allocator, scope, w9)
        asmLines += MOV(dest.asW, w9)
      }
      case Chr(e) => {
        asmLines += generateExpr(e, allocator, scope, dest.asW)
        asmLines += TST(dest.asW, ImmVal(0xffffff80))
        asmLines += CSEL(x1, dest, x1, Cond.NE) 
        asmLines += BCond(asmGlobal ~ P_ErrBadChar, Cond.NE)
        _predefinedFuncs += P_ErrBadChar
        _predefinedFuncs += P_Prints
      }
    }

    AsmFunction(asmLines.toList*)
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
      return (EmptyAsmSnippet, EmptyAsmSnippet)

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
