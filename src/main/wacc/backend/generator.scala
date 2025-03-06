package backend

import frontend.ast.*
import common.{Scope, SymbolTable}
import instructions.AsmLabeling.*
import allocator.*
import scala.collection.mutable.ListBuffer
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
            LabelledStringConst(asmLocal ~ f".str$index", str)
          })
          .toList*
      ),
      TextHeader(),
      GlobalHeader("main"),
      LabelHeader("main"),
      Comment("push {fp, lr}")(4),
      STP(fp, lr, PreIndex(sp, ImmVal(-16))),
      MOV(fp, sp),
      generateBlock(prog.s, RegisterMap(Seq(), 10), symbolTable.currentScope.children.head),
      Comment("pop {fp, lr}")(4),
      LDP(fp, lr, PreIndex(sp, ImmVal(-16))),
      RET
    )
    prog.fs.foreach(func => asmLines += generateFunc(func))
    _predefinedFuncs.foreach(name => asmLines += predefinedFunctions(name))
    AsmFunction(asmLines.toList*)
  }

  private def generateBlock(
      block: Stmt,
      _registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val w8 = WRegister(8)
    val stmts: List[Stmt] = block match {
      case Block(sts) => sts
      case _          => List()
    }

    var subScopes = scope.children

    // calculate the extra stack space needed for local variables within current scope
    // allocate register or stack space for locak variables within currentScope

    val offsetBefore: Int = ((_registerMap.stackOffset + 15) / 16) * 16
    val registerMap: RegisterMap = _registerMap :+ scope.localVars
    val offsetAfter: Int = ((registerMap.stackOffset + 15) / 16) * 16
    val extraStackSpace: Int = offsetAfter - offsetBefore

    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()

    if (extraStackSpace > 0)
      asmLines += SUBS(sp, sp, ImmVal(extraStackSpace))

    for (stmt <- stmts) {
      stmt match {

        case If(cond, b2, b1) => {
          val thenLabel = asmLocal ~ localLabelCount
          val afterLabel = asmLocal ~ (localLabelCount + 1)

          localLabelCount += 2
          asmLines += generateExpr(cond, registerMap, scope)
          asmLines += CBNZ(w8, thenLabel)
          // generate `else` block
          asmLines += generateBlock(b2, registerMap, subScopes.head)
          subScopes = subScopes.tail
          asmLines += B(afterLabel)

          // generate `then` block
          asmLines += LabelHeader(thenLabel)
          asmLines += generateBlock(b1, registerMap, subScopes.head)
          subScopes = subScopes.tail

          asmLines += LabelHeader(afterLabel)
        }

        case While(cond, block) => {
          val afterLabel = asmLocal ~ localLabelCount
          val loopLabel = asmLocal ~ (localLabelCount + 1)
          localLabelCount += 2

          asmLines += B(loopLabel)
          asmLines += LabelHeader(loopLabel)
          asmLines += generateBlock(block, registerMap, subScopes.head)
          subScopes = subScopes.tail
          asmLines += LabelHeader(afterLabel)
          asmLines += generateExpr(cond, registerMap, scope)
          asmLines += CBNZ(w8, loopLabel)
        }

        case Begin(block) => {
          asmLines += generateBlock(block, registerMap, subScopes.head)
          subScopes = subScopes.tail
        }

        case Exit(expr) => {
          asmLines += generateExpr(expr, registerMap, scope)
          asmLines += MOV(WRegister(0), WRegister(8))
          asmLines += BL("exit")
        }

        case Declare(ti, rvalue) => {
          asmLines += generateRValue(rvalue, registerMap, scope)
          val name: String = ti._2.name
          val varType = ti._1
          val location = registerMap(name)
          location._1 match {
            case reg: WRegister => asmLines += MOV(reg, WRegister(8))
            case reg: XRegister => asmLines += MOV(reg, XRegister(8))
            case offset: Int => {
              varType match {
                case BoolType() | CharType() =>
                  asmLines += STURB(WRegister(8), Offset(fp, ImmVal(offset)))
                case IntType() => asmLines += STUR(WRegister(8), Offset(fp, ImmVal(offset)))
                case _         => asmLines += STUR(XRegister(8), Offset(fp, ImmVal(offset)))
              }
            }
          }
        }

        case Assign(lValue, rValue) => {
          asmLines += generateRValue(rValue, registerMap, scope)
          asmLines += generateLValue(lValue, registerMap, scope)
        }

        case Return(expr) => {
          asmLines += generateExpr(expr, registerMap, scope)
          asmLines += MOV(XRegister(0), XRegister(8))
        }

        case PrintB(expr) => asmLines += generatePrint(expr, registerMap, scope, 'b')
        case PrintC(expr) => asmLines += generatePrint(expr, registerMap, scope, 'c')
        case PrintI(expr) => asmLines += generatePrint(expr, registerMap, scope, 'i')
        case PrintP(expr) => asmLines += generatePrint(expr, registerMap, scope, 'p')
        case PrintS(expr) => asmLines += generatePrint(expr, registerMap, scope, 's')

        case PrintlnB(expr) => asmLines += generatePrint(expr, registerMap, scope, 'b', true)
        case PrintlnC(expr) => asmLines += generatePrint(expr, registerMap, scope, 'c', true)
        case PrintlnI(expr) => asmLines += generatePrint(expr, registerMap, scope, 'i', true)
        case PrintlnP(expr) => asmLines += generatePrint(expr, registerMap, scope, 'p', true)
        case PrintlnS(expr) => asmLines += generatePrint(expr, registerMap, scope, 's', true)

        case FreeP(expr) => {
          val (pushCode, popCode) =
            pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)).to(ArrayBuffer))
          asmLines += AsmFunction(
            pushCode,
            generateExpr(expr, registerMap, scope),
            SUBS(XRegister(0), XRegister(8), ImmVal(4)),
            BL("free"),
            popCode
          )
          _predefinedFuncs += P_Freepair
        }

        case FreeA(expr) => {
          val (pushCode, popCode) =
            pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)).to(ArrayBuffer))
          asmLines += AsmFunction(
            pushCode,
            generateExpr(expr, registerMap, scope),
            BL("_freepair")
          )
        }

        case ReadC(lValue) => asmLines += generateRead(lValue, registerMap, scope, 'c')
        case ReadI(lValue) => asmLines += generateRead(lValue, registerMap, scope, 'i')

        case _ =>
      }
    }

    if (extraStackSpace > 0)
      asmLines += ADDS(sp, sp, ImmVal(extraStackSpace))

    AsmFunction(asmLines.toList*)
  }

  private def generatePrint(
      expr: Expr,
      registerMap: RegisterMap,
      scope: Scope,
      suffix: Char,
      newline: Boolean = false
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val (pushCode, popCode) =
      pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)).to(ArrayBuffer))
    val printFunc = suffix match {
      case 'b' => P_Printb
      case 'c' => P_Printc
      case 'i' => P_Printi
      case 'p' => P_Printp
      case 's' => P_Prints
      case _   => P_Printp
    }
    _predefinedFuncs += printFunc
    AsmFunction(
      pushCode,
      generateExpr(expr, registerMap, scope),
      MOV(XRegister(0), XRegister(8)),
      BL(f"_print${suffix}"),
      if newline then BL("_println") else EmptyAsmSnippet,
      popCode
    )
  }

  private def generateRead(
      lValue: LValue,
      registerMap: RegisterMap,
      scope: Scope,
      suffix: Char
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val (pushCode, popCode) =
      pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)).to(ArrayBuffer))
    _predefinedFuncs += (if suffix == 'i' then P_Readi else P_Readc)
    AsmFunction(
      pushCode,
      lValue match {
        case pairElem: PairElem => generateRValue(pairElem, registerMap, scope)
        case otherwise: Expr    => generateExpr(otherwise, registerMap, scope)
      },
      MOV(WRegister(0), WRegister(8)),
      BL(f"_read$suffix"),
      MOV(WRegister(8), WRegister(0)),
      popCode,
      generateLValue(lValue, registerMap, scope)
    )
  }

  private def generateFunc(func: Func)(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {

    val funcName: String = func.ti._2.name
    symbolTable.enterFunctionScope(funcName)

    // extract all parameters from symbolTable, allocate register or memory
    // a naive way of retrieving all parameters, can be modified
    val params =
      func.ps
        .fold(List())(x => x.ps)
        .map(x => (s"_func_${funcName}_params::" + x.i.name, TypeBridge.fromAst(x.t)))

    val numOfVariables = symbolTable.currentScope.maxConcurrentVars
    val calleeRegisters: ArrayBuffer[Register] =
      (1 to numOfVariables).map(n => XRegister(19 + n - 1)).to(ArrayBuffer)
    val (pushCode, popCode) = pushAndPopRegisters(calleeRegisters)
    val registerMap: RegisterMap = RegisterMap(params, numOfVariables)

    AsmFunction(
      LabelHeader(f"wacc_$funcName"),
      Comment("push {fp, lr}")(4),
      STP(fp, lr, PreIndex(sp, ImmVal(-16))),
      pushCode,
      MOV(fp, sp),
      // the current scope is for parameters
      generateBlock(func.s, registerMap, symbolTable.currentScope.children.head),
      popCode,
      Comment("pop {fp, lr}")(4),
      LDP(fp, lr, PreIndex(sp, ImmVal(-16))),
      RET
    )
  }

  /** Generate assembly code to evaluate the result of a rvalue.
    */
  private def generateRValue(
      rvalue: RValue,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    rvalue match {
      case Call(Ident(funcName), ArgList(argList)) => {
        val (pushCode, popCode) =
          pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)).to(ArrayBuffer))
        asmLines += pushCode
        val (argPushCode, offset) = pushArgs(funcName, argList, registerMap, scope)
        if (offset > 0)
          asmLines += SUBS(sp, sp, ImmVal(offset))
        asmLines += argPushCode
        asmLines += BL(asmGlobal ~ f"wacc_$funcName")
        asmLines += MOV(XRegister(8), XRegister(0))
        if (offset > 0)
          asmLines += ADDS(sp, sp, ImmVal(offset))
        asmLines += popCode
      }

      case ArrayLiterB(exprs) => asmLines += generateArrayLiter(exprs, 1, registerMap, scope)
      case ArrayLiterC(exprs) => asmLines += generateArrayLiter(exprs, 1, registerMap, scope)
      case ArrayLiterI(exprs) => asmLines += generateArrayLiter(exprs, 4, registerMap, scope)
      case ArrayLiterS(exprs) => asmLines += generateArrayLiter(exprs, 8, registerMap, scope)
      case ArrayLiterP(exprs) => asmLines += generateArrayLiter(exprs, 8, registerMap, scope)

      case NewPair(expr1, expr2) => {
        val (pushCode, popCode) =
          pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)).to(ArrayBuffer))
        asmLines += AsmFunction(
          pushCode,
          MOV(WRegister(0), ImmVal(16)),
          BL("_malloc"),
          MOV(ip0, XRegister(0)),
          popCode,
          generateExpr(expr1, registerMap, scope),
          STUR(XRegister(8), Offset(ip0, ImmVal(0))),
          generateExpr(expr2, registerMap, scope),
          STUR(XRegister(8), Offset(ip0, ImmVal(8))),
          MOV(XRegister(8), ip0)
        )
      }

      case First(lValue) =>
        asmLines += generatePairElemRValue(lValue, 0, registerMap, scope)

      case Second(lValue) =>
        asmLines += generatePairElemRValue(lValue, 8, registerMap, scope)

      case expr: Expr => asmLines += generateExpr(expr, registerMap, scope)

      case _ => throw Exception()
    }

    AsmFunction(asmLines.toList*)
  }

  private def generatePairElemRValue(
      lValue: LValue,
      offset: Int,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    lValue match {
      case pairElem: PairElem => asmLines += generateRValue(pairElem, registerMap, scope)
      case otherwise: Expr    => asmLines += generateExpr(otherwise, registerMap, scope)
    }
    asmLines += AsmFunction(
      CMP(XRegister(8), ImmVal(0)),
      BCond("_errNull", Cond.EQ),
      MOV(ip0, XRegister(8)),
      LDUR(XRegister(8), Offset(ip0, ImmVal(offset)))
    )
    AsmFunction(asmLines.toList*)
  }

  private def generateArrayLiter(
      exprs: List[Expr],
      typeSize: Int,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit symbolTable: SymbolTable): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val (pushCode, popCode) =
      pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)).to(ArrayBuffer))
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
      asmLines += generateExpr(expr, registerMap, scope)
      asmLines += STUR(WRegister(8), Offset(ip0, ImmVal(ind * typeSize)))
    }
    asmLines += MOV(XRegister(8), ip0)
    AsmFunction(asmLines.toList*)
  }

  /** Generate assembly code to move the content of x8 to a given location
    */
  private def generateLValue(
      lValue: LValue,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {

    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    lValue match {

      case Ident(name) => {
        val location = registerMap(name)
        val varType = scope.lookupSymbol(name).getOrElse(anyType)
        location._1 match {
          case reg: WRegister => asmLines += MOV(reg, WRegister(8))
          case reg: XRegister => asmLines += MOV(reg, XRegister(8))
          case offset: Int => {
            varType match {
              case BoolType() | CharType() =>
                asmLines += STURB(WRegister(8), Offset(fp, ImmVal(offset)))
              case _ => asmLines += STUR(XRegister(8), Offset(fp, ImmVal(offset)))
            }
          }
        }
      }
      // TODO: Array 
      case ArrayElem(Ident(name), exprs) => ???

      case First(lValue)  => generatePairElemLValue(lValue, 0, registerMap, scope)
      case Second(lValue) => generatePairElemLValue(lValue, 8, registerMap, scope)

      case _ => throw Exception()
    }
    AsmFunction(asmLines.toList*)
  }

  private def generatePairElemLValue(
      lValue: LValue,
      offset: Int,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val x8 = XRegister(8)
    val x9 = XRegister(9)
    asmLines += STP(XRegister(8), xzr, PreIndex(sp, ImmVal(-16)))
    lValue match {
      case pairElem: PairElem => asmLines += generateRValue(pairElem, registerMap, scope)
      case otherwise: Expr    => asmLines += generateExpr(otherwise, registerMap, scope)
    }
    asmLines += MOV(x9, x8)
    asmLines += CMP(x9, ImmVal(0))
    asmLines += BCond("_errNull", Cond.EQ)
    asmLines += LDP(x8, xzr, PostIndex(sp, ImmVal(16)))
    asmLines += STR(x8, Offset(x9, ImmVal(offset)))
    AsmFunction(asmLines.toList*)
  }

  /** Generate assembly code to evaluate the result of an expression The default position of result
    * is register X8
    */
  private def generateExpr(
      expr: Expr,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val x8 = XRegister(8)
    val w8 = WRegister(8)
    expr match {
      case IntLiter(x)  => asmLines += MOV(w8, ImmVal(x))
      case BoolLiter(x) => asmLines += MOV(w8, ImmVal(if (x) then 1 else 0))
      case CharLiter(c) => asmLines += MOV(w8, ImmVal(c))
      case StrLiter(s) => {
        var index = _stringConsts.size
        if (_stringConsts.contains(s)) {
          index = _stringConsts(s)
        } else {
          _stringConsts(s) = index
        }
        asmLines += ADRP(x8, asmLocal ~ f".str$index")
        asmLines += ADD(x8, x8, Lo12(asmLocal ~ f".str$index"))
      }
      case PairLiter() => asmLines += MOV(x8, ImmVal(0))
      case Ident(name) => {
        registerMap(name)._1 match
          case reg: WRegister => asmLines += MOV(w8, reg)
          case reg: XRegister => asmLines += MOV(x8, reg)
          case offset: Int   => asmLines += LDUR(x8, Offset(fp, ImmVal(offset)))
      }
      case ArrayElem(Ident(name), exprs) => {

        // Assume that the array pointer is at x8
        def unwrap(varType: WaccType, exprs: List[Expr]): AsmSnippet = {

          val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
          // generate array index and move to ip1
          asmLines += STP(XRegister(8), xzr, PreIndex(sp, ImmVal(-16)))
          asmLines += generateExpr(exprs.head, registerMap, scope)
          asmLines += MOV(ip1, XRegister(8))
          asmLines += LDP(XRegister(8), xzr, PostIndex(sp, ImmVal(16)))

          asmLines += STP(XRegister(7), xzr, PreIndex(sp, ImmVal(-16)))
          asmLines += MOV(XRegister(7), XRegister(8))

          var subIndexing: AsmSnippet = EmptyAsmSnippet
          varType match {
            case ArrayType(insideType) =>
              _predefinedFuncs += P_ArrLoad8
              asmLines += BL("_arrLoad8")
              subIndexing = unwrap(insideType, exprs.tail)
            case x: IntType =>
              _predefinedFuncs += P_ArrLoad4
              asmLines += BL("_arrLoad4")
            case x: (BoolType | CharType) =>
              _predefinedFuncs += P_ArrLoad1
              asmLines += BL("_arrLoad1")
            case _ =>
              _predefinedFuncs += P_ArrLoad8
              asmLines += BL("_arrLoad8")
          }
          asmLines += MOV(XRegister(8), XRegister(7))
          asmLines += LDP(XRegister(7), xzr, PostIndex(sp, ImmVal(16)))
          asmLines += subIndexing
          AsmFunction(asmLines.toList*)
        }
        asmLines += generateExpr(Ident(name)((-1, -1)), registerMap, scope)
        val arrayType = scope.lookupSymbol(name).get
        asmLines += unwrap(arrayType, exprs)
      }
      case Paren(e)   => generateExpr(e, registerMap, scope)
      case e: UnaryOp => asmLines += generateUnary(e, registerMap, scope) // Unary Operations
      case e: BinaryOp =>
        asmLines += generateBinary(e, registerMap, scope) // Binary Operations
    }

    AsmFunction(asmLines.toList*)
  }

  private def generateBinary(
      binaryOp: BinaryOp,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    binaryOp match {
      case Or(expr1, expr2) =>
        asmLines += generateLogical(expr1, expr2, Cond.EQ, registerMap, scope)
      case And(expr1, expr2) =>
        asmLines += generateLogical(expr1, expr2, Cond.NE, registerMap, scope)

      case Equal(expr1, expr2) =>
        asmLines += generateComp(expr1, expr2, Cond.EQ, registerMap, scope)
      case NotEqual(expr1, expr2) =>
        asmLines += generateComp(expr1, expr2, Cond.NE, registerMap, scope)
      case Less(expr1, expr2) => asmLines += generateComp(expr1, expr2, Cond.LT, registerMap, scope)
      case LessEqual(expr1, expr2) =>
        asmLines += generateComp(expr1, expr2, Cond.LE, registerMap, scope)
      case Greater(expr1, expr2) =>
        asmLines += generateComp(expr1, expr2, Cond.GT, registerMap, scope)
      case GreaterEqual(expr1, expr2) =>
        asmLines += generateComp(expr1, expr2, Cond.GE, registerMap, scope)

      case Add(expr1, expr2) =>
        asmLines += generateArithmetic1(expr1, expr2, "ADD", registerMap, scope)
      case Sub(expr1, expr2) =>
        asmLines += generateArithmetic1(expr1, expr2, "SUB", registerMap, scope)
      case Mul(expr1, expr2) =>
        asmLines += generateArithmetic1(expr1, expr2, "MUL", registerMap, scope)

      case Div(expr1, expr2) =>
        asmLines += generateArithmetic2(expr1, expr2, "DIV", registerMap, scope)
      case Mod(expr1, expr2) =>
        asmLines += generateArithmetic2(expr1, expr2, "MOD", registerMap, scope)

    }

    AsmFunction(asmLines.toList*)
  }

  private def generateLogical(
      expr1: Expr,
      expr2: Expr,
      cond: Cond,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val w8 = WRegister(8)
    val label = asmLocal ~ localLabelCount
    localLabelCount += 1

    asmLines += generateExpr(expr1, registerMap, scope)
    asmLines += CMP(w8, ImmVal(1))
    asmLines += BCond(label, cond)
    asmLines += generateExpr(expr2, registerMap, scope)
    asmLines += CMP(w8, ImmVal(1))
    asmLines += LabelHeader(label)
    asmLines += CSET(w8, Cond.EQ)

    AsmFunction(asmLines.toList*)
  }

  private def generateComp(
      expr1: Expr,
      expr2: Expr,
      cond: Cond,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val w8 = WRegister(8)
    val w9 = WRegister(9)
    AsmFunction(
      generateExpr(expr1, registerMap, scope),
      MOV(w9, w8),
      generateExpr(expr2, registerMap, scope),
      CMP(w8, w9),
      CSET(w8, cond)
    )
  }

  private def generateArithmetic1(
      expr1: Expr,
      expr2: Expr,
      operation: String,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val w8 = WRegister(8)
    val x8 = XRegister(8)
    val w9 = WRegister(9)
    val x9 = XRegister(9)

    asmLines += generateExpr(expr1, registerMap, scope)
    asmLines += STP(x8, xzr, PreIndex(sp, ImmVal(-16)))
    asmLines += generateExpr(expr2, registerMap, scope)
    asmLines += LDP(x9, xzr, PostIndex(sp, ImmVal(16)))

    operation match {
      case "ADD" =>
        asmLines += ADDS(w8, w9, w8)
        asmLines += BCond(asmGlobal ~ "_errOverflow", Cond.VS)
      case "SUB" =>
        asmLines += SUBS(w8, w9, w8)
        asmLines += BCond(asmGlobal ~ "_errOverflow", Cond.VS)
      case "MUL" =>
        asmLines += SMULL(x8, w9, w8)
        asmLines += CMP(x8, w8, Some(SXTW()))
        asmLines += BCond(asmGlobal ~ "_errOverflow", Cond.NE)
    }

    _predefinedFuncs += P_ErrOverflow
    _predefinedFuncs += P_Prints

    AsmFunction(asmLines.toList*)
  }

  private def generateArithmetic2(
      expr1: Expr,
      expr2: Expr,
      operation: String,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val w8 = WRegister(8)
    val w9 = WRegister(9)

    asmLines += generateExpr(expr2, registerMap, scope)
    asmLines += CMP(w8, ImmVal(0))
    asmLines += BCond(asmGlobal ~ "_errDivZero", Cond.EQ)
    _predefinedFuncs += P_ErrDivZero
    _predefinedFuncs += P_Prints
    asmLines += MOV(w9, w8)
    asmLines += generateExpr(expr1, registerMap, scope)

    operation match {
      case "DIV" =>
        asmLines += SDIV(w8, w8, w9)
      case "MOD" =>
        asmLines += SDIV(ip1.asW, w8, w9)
        asmLines += MSUB(w8, ip1.asW, w9, w8)
    }

    AsmFunction(asmLines.toList*)
  }

  private def generateUnary(
      unaryOp: UnaryOp,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: SymbolTable
  ): AsmSnippet = {
    val asmLines: ListBuffer[AsmSnippet] = ListBuffer()
    val x1 = XRegister(1)
    val w8 = WRegister(8)
    val x8 = XRegister(8)
    val x9 = WRegister(9)
    val w9 = WRegister(9)

    unaryOp match {
      case Not(e) => {
        asmLines += generateExpr(e, registerMap, scope)
        asmLines += MOV(w9, w8)
        asmLines += CMP(w9, ImmVal(1))
        asmLines += CSET(w8, Cond.NE)
      }
      case Negate(e) => {
        asmLines += generateExpr(e, registerMap, scope)
        asmLines += MOV(w9, w8)
        asmLines += NEGS(w8, w9);
        asmLines += BCond(asmGlobal ~ "_errOverflow", Cond.VS)
        _predefinedFuncs += P_ErrOverflow
        _predefinedFuncs += P_Prints
      }
      case Len(e) => {
        asmLines += generateExpr(e, registerMap, scope)
        asmLines += MOV(w9, w8)
        asmLines += LDUR(w8, Offset(x9, ImmVal(-4)))
      }
      case Ord(e) => {
        asmLines += generateExpr(e, registerMap, scope)
      }
      case Chr(e) => {
        asmLines += generateExpr(e, registerMap, scope)
        asmLines += TST(w8, ImmVal(0xffffff80))
        asmLines += CSEL(x1, x8, x1, Cond.NE)
        asmLines += BCond(asmGlobal ~ "_errBadChar", Cond.NE)
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
      registerMap: RegisterMap,
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
        asmLines += generateExpr(expr, registerMap, scope)
      } else {
        asmLines += generateExpr(expr, registerMap, scope)
        asmLines += STR(dest, Offset(sp, ImmVal(offset)))
        offset += paramSize
        paramCount -= 1
      }
      paramCount += 1
    }
    (AsmFunction(asmLines.toList*), math.floorDiv(offset + 15, 16) * 16)
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
