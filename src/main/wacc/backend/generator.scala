package backend

import frontend.ast.*
import common.{Scope, FrozenSymbolTable}
import instructions.AsmLabeling.*
import allocator.*
import scala.collection.mutable.Map as MutableMap
import scala.collection.mutable.Set as MutableSet
import instructions.PredefinedFunctions.*
import instructions.*
object Generator:
  private var localLabelCount: Int = 0
  private var stringConsts: MutableMap[String, Int] = MutableMap()
  private var predefFuncs: MutableSet[PredefinedFunc] = MutableSet()

  private def addPredefFunc(f: PredefinedFunc) = predefFuncs.add(f)

  private def reset() =
    localLabelCount = 0
    stringConsts = MutableMap()
    predefFuncs = MutableSet()

  def generate(prog: Program)(implicit symbolTable: FrozenSymbolTable): String =
    reset()
    val main = generateMain(prog.s)
    val funcs =
      prog.fs
        .map:
          case f @ Func((_, Ident(name)), _, _) =>
            generateFunc(f, symbolTable.getFuncScope(name))
        .toSeq
    val generatedCode: StringBuilder = StringBuilder()
    generatedCode.appendAll(
      DataHeader(),
      join(
        stringConsts
          .map: (str, index) =>
            LabelledStringConst(asmLocal ~ f".str$index", str)
          .toSeq*
      ),
      main,
      join(funcs*),
      join(
        predefFuncs.map(f => predefinedFunctions(f)).toSeq*
      )
    )
    generatedCode.toString

  private def generateMain(mainBlock: Stmt)(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    join(
      TextHeader(),
      GlobalHeader("main"),
      LabelHeader("main"),
      Comment("push {fp, lr}")(4),
      STP(fp, lr, PreIndex(sp, ImmVal(-16))),
      MOV(fp, sp),
      generateBlock(mainBlock, RegisterMap(Nil, 10), symbolTable.mainScope),
      MOV(XRegister(0), ImmVal(0)),
      Comment("pop {fp, lr}")(4),
      LDP(fp, lr, PostIndex(sp, ImmVal(16))),
      RET
    )

  private def generateBlock(
      block: Stmt,
      inheritedRegisterMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    val w8 = WRegister(8)
    val stmts: Seq[Stmt] = block match
      case Block(sts) => sts
      case _          => Nil

    var subScopes = scope.children.toList

    // calculate the extra stack space needed for local variables within current scope
    // allocate register or stack space for locak variables within currentScope
    val offsetBefore: Int = ((inheritedRegisterMap.stackOffset + 15) / 16) * 16
    val registerMap: RegisterMap = inheritedRegisterMap :+ scope.localVars
    val offsetAfter: Int = ((registerMap.stackOffset + 15) / 16) * 16
    val extraStackSpace: Int = offsetAfter - offsetBefore

    val generatedCode: StringBuilder = StringBuilder()

    if (extraStackSpace > 0)
      generatedCode.appendAll(
        SUB(sp, sp, ImmVal(extraStackSpace))
      )

    stmts.foreach:
      case If(cond, b2, b1) =>
        val thenLabel = asmLocal ~ localLabelCount
        val finallyLabel = asmLocal ~ (localLabelCount + 1)

        localLabelCount += 2
        generatedCode.appendAll(
          generateExpr(cond, registerMap, scope),
          CBNZ(w8, thenLabel),
          // generate `else` block
          generateBlock(b2, registerMap, subScopes(0)),
          B(finallyLabel),
          // generate `then` block
          LabelHeader(thenLabel),
          generateBlock(b1, registerMap, subScopes(1)),
          LabelHeader(finallyLabel)
        )
        subScopes = subScopes.drop(2)

      case While(cond, block) =>
        val finallyLabel = asmLocal ~ localLabelCount
        val loopLabel = asmLocal ~ (localLabelCount + 1)
        localLabelCount += 2

        generatedCode.appendAll(
          B(finallyLabel),
          LabelHeader(loopLabel),
          generateBlock(block, registerMap, subScopes.head),
          LabelHeader(finallyLabel),
          generateExpr(cond, registerMap, scope),
          CMP(w8, ImmVal(1)),
          CBNZ(w8, loopLabel)
        )

        subScopes = subScopes.tail

      case Begin(block) =>
        generatedCode.appendAll(
          generateBlock(block, registerMap, subScopes.head)
        )
        subScopes = subScopes.tail

      case Exit(expr) =>
        generatedCode.appendAll(
          generateExpr(expr, registerMap, scope),
          MOV(WRegister(0), WRegister(8)),
          BL("exit")
        )

      case Declare((t, Ident(name)), rvalue) =>
        generatedCode.appendAll(
          generateRValue(rvalue, registerMap, scope),
          registerMap(name) match
            case (reg: XRegister, byteSize: Int) => MOV(reg, XRegister(8))
            case (reg: WRegister, byteSize: Int) => MOV(reg, WRegister(8))
            case (offset: Int, byteSize: Int) =>
              byteSize match
                case 1 => STURB(WRegister(8), Offset(fp, ImmVal(offset)))
                case 4 => STUR(WRegister(8), Offset(fp, ImmVal(offset)))
                case _ => STUR(XRegister(8), Offset(fp, ImmVal(offset)))
        )

      case Assign(lValue, rValue) =>
        generatedCode.appendAll(
          generateRValue(rValue, registerMap, scope),
          generateLValue(lValue, registerMap, scope)
        )

      case Return(expr) =>
        generatedCode.appendAll(
          generateExpr(expr, registerMap, scope),
          MOV(XRegister(0), XRegister(8))
        )

      case PrintB(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 'b')
      case PrintC(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 'c')
      case PrintI(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 'i')
      case PrintP(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 'p')
      case PrintS(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 's')

      case PrintlnB(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 'b', true)
      case PrintlnC(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 'c', true)
      case PrintlnI(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 'i', true)
      case PrintlnP(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 'p', true)
      case PrintlnS(expr) => generatedCode ++= generatePrint(expr, registerMap, scope, 's', true)

      case FreeP(expr) =>
        val (pushCode, popCode) =
          pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)))
        generatedCode.appendAll(
          pushCode,
          generateExpr(expr, registerMap, scope),
          SUBS(XRegister(0), XRegister(8), ImmVal(4)),
          BL("free"),
          popCode
        )
        addPredefFunc(P_Freepair)

      case FreeA(expr) =>
        val (pushCode, popCode) =
          pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)))
        generatedCode.appendAll(
          pushCode,
          generateExpr(expr, registerMap, scope),
          BL("_freepair")
        )

      case ReadC(lValue) => generatedCode ++= generateRead(lValue, registerMap, scope, 'c')
      case ReadI(lValue) => generatedCode ++= generateRead(lValue, registerMap, scope, 'i')

      case _ =>

    if (extraStackSpace > 0)
      generatedCode.appendAll(
        ADD(sp, sp, ImmVal(extraStackSpace))
      )

    generatedCode

  private def generatePrint(
      expr: Expr,
      registerMap: RegisterMap,
      scope: Scope,
      suffix: Char,
      newline: Boolean = false
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    val (pushCode, popCode) =
      pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)))
    if newline then predefFuncs += P_Println
    val printFunc = suffix match
      case 'b' => P_Printb
      case 'c' => P_Printc
      case 'i' => P_Printi
      case 'p' => P_Printp
      case 's' => P_Prints
      case _   => P_Printp
    predefFuncs += printFunc
    join(
      pushCode,
      generateExpr(expr, registerMap, scope),
      MOV(XRegister(0), XRegister(8)),
      BL(f"_print${suffix}"),
      if newline then BL("_println") else EmptyAsmSnippet,
      popCode
    )

  private def generateRead(
      lValue: LValue,
      registerMap: RegisterMap,
      scope: Scope,
      suffix: Char
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    val (pushCode, popCode) =
      pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)))
    predefFuncs += (if suffix == 'i' then P_Readi else P_Readc)
    join(
      pushCode,
      lValue match
        case pairElem: PairElem => generateRValue(pairElem, registerMap, scope)
        case expr: Expr         => generateExpr(expr, registerMap, scope)
      ,
      MOV(WRegister(0), WRegister(8)),
      BL(f"_read$suffix"),
      MOV(WRegister(8), WRegister(0)),
      popCode,
      generateLValue(lValue, registerMap, scope)
    )

  private def generateFunc(func: Func, funcScope: Scope)(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =

    val funcName: String = func.ti._2.name

    // extract all parameters from symbolTable, allocate register or memory
    // a naive way of retrieving all parameters, can be modified
    val params =
      func.ps
        .fold(List())(x => x.ps)
        .map(x => (s"_func_${funcName}_params::" + x.i.name, TypeBridge.fromAst(x.t)))

    val numOfVariables = funcScope.maxConcurrentVars
    val calleeRegisters: Seq[Register] = (1 to numOfVariables).map(n => XRegister(19 + n - 1))
    val (pushCode, popCode) = pushAndPopRegisters(calleeRegisters)
    val registerMap: RegisterMap = RegisterMap(params, numOfVariables)

    join(
      LabelHeader(f"wacc_$funcName"),
      Comment("push {fp, lr}")(4),
      STP(fp, lr, PreIndex(sp, ImmVal(-16))),
      pushCode,
      MOV(fp, sp),
      // the current scope is for parameters
      generateBlock(func.s, registerMap, funcScope),
      popCode,
      Comment("pop {fp, lr}")(4),
      LDP(fp, lr, PreIndex(sp, ImmVal(-16))),
      RET
    )

  /** Generate assembly code to evaluate the result of a rvalue.
    */
  private def generateRValue(
      rvalue: RValue,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    val generatedCode: StringBuilder = StringBuilder()
    rvalue match {
      case Call(Ident(funcName), ArgList(argList)) =>
        val (pushCode, popCode) = pushAndPopRegisters(
          registerMap.usedCallerRegisters.map(XRegister(_))
        )
        val (argPushCode, offset) = pushArgs(funcName, argList, registerMap, scope)
        generatedCode.appendAll(
          pushCode,
          if (offset > 0) then SUB(sp, sp, ImmVal(offset)) else EmptyAsmSnippet,
          argPushCode,
          BL(asmGlobal ~ f"wacc_$funcName"),
          MOV(XRegister(8), XRegister(0)),
          if (offset > 0) then ADD(sp, sp, ImmVal(offset)) else EmptyAsmSnippet,
          popCode
        )

      case ArrayLiter(exprs)  => generatedCode ++= generateArrayLiter(exprs, 0, registerMap, scope)
      case ArrayLiterB(exprs) => generatedCode ++= generateArrayLiter(exprs, 1, registerMap, scope)
      case ArrayLiterC(exprs) => generatedCode ++= generateArrayLiter(exprs, 1, registerMap, scope)
      case ArrayLiterI(exprs) => generatedCode ++= generateArrayLiter(exprs, 4, registerMap, scope)
      case ArrayLiterS(exprs) => generatedCode ++= generateArrayLiter(exprs, 8, registerMap, scope)
      case ArrayLiterP(exprs) => generatedCode ++= generateArrayLiter(exprs, 8, registerMap, scope)

      case NewPair(expr1, expr2) =>
        val (pushCode, popCode) = pushAndPopRegisters(
          registerMap.usedCallerRegisters.map(XRegister(_))
        )
        addPredefFunc(P_Malloc)
        addPredefFunc(P_ErrOutOfMemory)
        addPredefFunc(P_Prints)
        generatedCode.appendAll(
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

      case First(lValue) =>
        generatedCode ++= generatePairElemRValue(lValue, 0, registerMap, scope)

      case Second(lValue) =>
        generatedCode ++= generatePairElemRValue(lValue, 1, registerMap, scope)

      case expr: Expr => generatedCode ++= generateExpr(expr, registerMap, scope)

      case _ =>
    }

    generatedCode

  private def generatePairElemRValue(
      lValue: LValue,
      offset: Int,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    addPredefFunc(P_ErrNull)
    addPredefFunc(P_Prints)
    join(
      lValue match
        case pairElem: PairElem => generateRValue(pairElem, registerMap, scope)
        case otherwise: Expr    => generateExpr(otherwise, registerMap, scope)
      ,
      CMP(XRegister(8), ImmVal(0)),
      BCond("_errNull", Cond.EQ),
      MOV(ip0, XRegister(8)),
      LDUR(XRegister(8), Offset(ip0, ImmVal(offset)))
    )

  private def generateArrayLiter(
      exprs: List[Expr],
      typeSize: Int,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit symbolTable: FrozenSymbolTable): StringBuilder =
    val generatedCode: StringBuilder = StringBuilder()
    val (pushCode, popCode) =
      pushAndPopRegisters(registerMap.usedCallerRegisters.map(XRegister(_)))
    val arrayLen = exprs.length
    addPredefFunc(P_Malloc)
    addPredefFunc(P_ErrOutOfMemory)
    addPredefFunc(P_Prints)

    generatedCode.appendAll(
      pushCode,
      MOV(WRegister(0), ImmVal(4 + typeSize * arrayLen)),
      BL("_malloc"),
      MOV(ip0, XRegister(0)),
      popCode,
      ADDS(ip0, ip0, ImmVal(4)),
      MOV(WRegister(8), ImmVal(arrayLen)),
      STUR(WRegister(8), Offset(ip0, ImmVal(-4)))
    )
    exprs.zipWithIndex.map: (expr, ind) =>
      generatedCode.appendAll(
        generateExpr(expr, registerMap, scope),
        STUR(WRegister(8), Offset(ip0, ImmVal(ind * typeSize)))
      )
    generatedCode.appendAll(MOV(XRegister(8), ip0))

  /** Generate assembly code to move the content of x8 to a given location
    */
  private def generateLValue(
      lValue: LValue,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    join(lValue match

      case Ident(name) =>
        val location = registerMap(name)
        val varType = scope.lookupSymbol(name).getOrElse(anyType)
        join(location._1 match
          case reg: WRegister => MOV(reg, WRegister(8))
          case reg: XRegister => MOV(reg, XRegister(8))
          case offset: Int =>
            varType match
              case BoolType() | CharType() =>
                STURB(WRegister(8), Offset(fp, ImmVal(offset)))
              case _ => STUR(XRegister(8), Offset(fp, ImmVal(offset)))
        )

      case ArrayElem(Ident(name), exprs) =>
        generateArrayElem(registerMap, scope, name, exprs, "Store")

      case First(lValue)  => generatePairElemLValue(lValue, 0, registerMap, scope)
      case Second(lValue) => generatePairElemLValue(lValue, 8, registerMap, scope)
      case _              => throw Exception()
    )

  private def generatePairElemLValue(
      lValue: LValue,
      offset: Int,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    val x8 = XRegister(8)
    val x9 = XRegister(9)
    addPredefFunc(P_ErrNull)
    addPredefFunc(P_Prints)
    join(
      STP(XRegister(8), xzr, PreIndex(sp, ImmVal(-16))),
      lValue match
        case pairElem: PairElem => generateRValue(pairElem, registerMap, scope)
        case otherwise: Expr    => generateExpr(otherwise, registerMap, scope)
      ,
      MOV(x9, x8),
      CMP(x9, ImmVal(0)),
      BCond("_errNull", Cond.EQ),
      LDP(x8, xzr, PostIndex(sp, ImmVal(16))),
      STR(x8, Offset(x9, ImmVal(offset)))
    )

  /** Generate assembly code to evaluate the result of an expression The default position of result
    * is register X8
    */
  private def generateExpr(
      expr: Expr,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    val x8 = XRegister(8)
    val w8 = WRegister(8)
    join(
      expr match
        case IntLiter(x)  => MOV(w8, ImmVal(x))
        case BoolLiter(x) => MOV(w8, ImmVal(if (x) then 1 else 0))
        case CharLiter(c) => MOV(w8, ImmVal(c))
        case StrLiter(s) =>
          var index = stringConsts.size
          if (stringConsts.contains(s)) then index = stringConsts(s)
          else stringConsts(s) = index
          join(
            ADRP(x8, asmLocal ~ f".str$index"),
            ADD(x8, x8, Lo12(asmLocal ~ f".str$index"))
          )
        case PairLiter() => join(MOV(x8, ImmVal(0)))
        case Ident(name) =>
          registerMap(name) match
            case (reg: XRegister, _) => MOV(x8, reg)
            case (reg: WRegister, _) => MOV(w8, reg)
            case (offset: Int, _)    => LDUR(x8, Offset(fp, ImmVal(offset)))
        case ArrayElem(Ident(name), exprs) =>
          generateArrayElem(registerMap, scope, name, exprs, "Load")
        case Paren(e)    => generateExpr(e, registerMap, scope)
        case e: UnaryOp  => generateUnary(e, registerMap, scope)
        case e: BinaryOp => generateBinary(e, registerMap, scope)
    )

  private def generateArrayElem(
      registerMap: RegisterMap,
      scope: Scope,
      name: String,
      exprs: List[Expr],
      mode: String
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =

    // Assume that the array pointer is at x7
    def unwrap(varType: WaccType, exprs: List[Expr]): StringBuilder =
      val generatedCode: StringBuilder = StringBuilder()
      addPredefFunc(P_ErrOutOfBounds)
      addPredefFunc(P_Prints)

      generatedCode.appendAll(
        STP(XRegister(8), xzr, PreIndex(sp, ImmVal(-16))),
        generateExpr(exprs.head, registerMap, scope),
        MOV(ip1, XRegister(8)),
        LDP(XRegister(8), xzr, PostIndex(sp, ImmVal(16))),
        if exprs.tail.isEmpty then
          TypeBridge.fromAst(varType).byteSize match
            case 8 =>
              predefFuncs += (if mode == "Load" then P_ArrLoad8 else P_ArrStore8)
              BL(f"_arr${mode}8")
            case 4 =>
              predefFuncs += (if mode == "Load" then P_ArrLoad4 else P_ArrStore4)
              BL(f"_arr${mode}4")
            case _ =>
              predefFuncs += (if mode == "Load" then P_ArrLoad1 else P_ArrStore1)
              BL(f"_arr${mode}1")
        else
          varType match
            case ArrayType(insideType) =>
              predefFuncs += P_ArrLoad8
              join(
                BL("_arrLoad8"),
                unwrap(insideType, exprs.tail)
              )
            case x: IntType =>
              predefFuncs += P_ArrLoad4
              BL("_arrLoad4")
            case x: (BoolType | CharType) =>
              predefFuncs += P_ArrLoad1
              BL("_arrLoad1")
            case _ =>
              predefFuncs += P_ArrLoad8
              BL("_arrLoad8")
      )
    join(
      STP(XRegister(7), xzr, PreIndex(sp, ImmVal(-16))),
      STP(XRegister(8), xzr, PreIndex(sp, ImmVal(-16))),
      generateExpr(Ident(name)(defaultPos), registerMap, scope),
      MOV(XRegister(7), XRegister(8)),
      LDP(XRegister(8), xzr, PostIndex(sp, ImmVal(16))),
      scope.varTable(name) match {
        case ArrayType(insideType) => unwrap(insideType, exprs)
        case _                     => throw Exception()
      },
      MOV(XRegister(8), XRegister(7)),
      LDP(XRegister(7), xzr, PostIndex(sp, ImmVal(16)))
    )

  private def generateBinary(
      binaryOp: BinaryOp,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    join(
      binaryOp match
        case Or(expr1, expr2) =>
          generateLogical(expr1, expr2, Cond.EQ, registerMap, scope)
        case And(expr1, expr2) =>
          generateLogical(expr1, expr2, Cond.NE, registerMap, scope)

        case Equal(expr1, expr2) =>
          generateComp(expr1, expr2, Cond.EQ, registerMap, scope)
        case NotEqual(expr1, expr2) =>
          generateComp(expr1, expr2, Cond.NE, registerMap, scope)
        case Less(expr1, expr2) =>
          generateComp(expr1, expr2, Cond.LT, registerMap, scope)
        case LessEqual(expr1, expr2) =>
          generateComp(expr1, expr2, Cond.LE, registerMap, scope)
        case Greater(expr1, expr2) =>
          generateComp(expr1, expr2, Cond.GT, registerMap, scope)
        case GreaterEqual(expr1, expr2) =>
          generateComp(expr1, expr2, Cond.GE, registerMap, scope)

        case Add(expr1, expr2) =>
          generateArithmetic1(expr1, expr2, "ADD", registerMap, scope)
        case Sub(expr1, expr2) =>
          generateArithmetic1(expr1, expr2, "SUB", registerMap, scope)
        case Mul(expr1, expr2) =>
          generateArithmetic1(expr1, expr2, "MUL", registerMap, scope)

        case Div(expr1, expr2) =>
          generateArithmetic2(expr1, expr2, "DIV", registerMap, scope)
        case Mod(expr1, expr2) =>
          generateArithmetic2(expr1, expr2, "MOD", registerMap, scope)
    )

  private def generateLogical(
      expr1: Expr,
      expr2: Expr,
      cond: Cond,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    val w8 = WRegister(8)
    val label = asmLocal ~ localLabelCount
    localLabelCount += 1

    join(
      generateExpr(expr1, registerMap, scope),
      CMP(w8, ImmVal(1)),
      BCond(label, cond),
      generateExpr(expr2, registerMap, scope),
      CMP(w8, ImmVal(1)),
      LabelHeader(label),
      CSET(w8, Cond.EQ)
    )

  private def generateComp(
      expr1: Expr,
      expr2: Expr,
      cond: Cond,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    val w8 = WRegister(8)
    val w9 = WRegister(9)
    join(
      generateExpr(expr1, registerMap, scope),
      MOV(w9, w8),
      generateExpr(expr2, registerMap, scope),
      CMP(w9, w8),
      CSET(w8, cond)
    )

  private def generateArithmetic1(
      expr1: Expr,
      expr2: Expr,
      operation: String,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    addPredefFunc(P_ErrOverflow)
    addPredefFunc(P_Prints)
    val w8 = WRegister(8)
    val x8 = XRegister(8)
    val w9 = WRegister(9)
    val x9 = XRegister(9)

    join(
      generateExpr(expr1, registerMap, scope),
      STP(x8, xzr, PreIndex(sp, ImmVal(-16))),
      generateExpr(expr2, registerMap, scope),
      LDP(x9, xzr, PostIndex(sp, ImmVal(16))),
      operation match
        case "ADD" =>
          join(
            ADDS(w8, w9, w8),
            BCond(asmGlobal ~ "_errOverflow", Cond.VS)
          )
        case "SUB" =>
          join(
            SUBS(w8, w9, w8),
            BCond(asmGlobal ~ "_errOverflow", Cond.VS)
          )
        case "MUL" =>
          join(
            SMULL(x8, w9, w8),
            CMP(x8, w8, Some(SXTW())),
            BCond(asmGlobal ~ "_errOverflow", Cond.NE)
          )
    )

  private def generateArithmetic2(
      expr1: Expr,
      expr2: Expr,
      operation: String,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    addPredefFunc(P_ErrDivZero)
    addPredefFunc(P_Prints)
    val w8 = WRegister(8)
    val w9 = WRegister(9)

    join(
      generateExpr(expr2, registerMap, scope),
      CMP(w8, ImmVal(0)),
      BCond(asmGlobal ~ "_errDivZero", Cond.EQ),
      MOV(w9, w8),
      generateExpr(expr1, registerMap, scope),
      operation match
        case "DIV" =>
          SDIV(w8, w8, w9)
        case "MOD" =>
          join(SDIV(ip1.asW, w8, w9), MSUB(w8, ip1.asW, w9, w8))
    )

  private def generateUnary(
      unaryOp: UnaryOp,
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): StringBuilder =
    val x1 = XRegister(1)
    val w8 = WRegister(8)
    val x8 = XRegister(8)
    val x9 = XRegister(9)
    val w9 = WRegister(9)

    join(unaryOp match
      case Not(e) =>
        join(
          generateExpr(e, registerMap, scope),
          MOV(w9, w8),
          CMP(w9, ImmVal(1)),
          CSET(w8, Cond.NE)
        )
      case Negate(e) =>
        addPredefFunc(P_ErrOverflow)
        addPredefFunc(P_Prints)
        join(
          generateExpr(e, registerMap, scope),
          MOV(w9, w8),
          NEGS(w8, w9),
          BCond(asmGlobal ~ "_errOverflow", Cond.VS)
        )
      case Len(e) =>
        join(
          generateExpr(e, registerMap, scope),
          MOV(w9, w8),
          LDUR(w8, Offset(x9, ImmVal(-4)))
        )
      case Ord(e) => generateExpr(e, registerMap, scope)
      case Chr(e) =>
        addPredefFunc(P_ErrBadChar)
        addPredefFunc(P_Prints)
        join(
          generateExpr(e, registerMap, scope),
          TST(w8, ImmVal(0xffffff80)),
          CSEL(x1, x8, x1, Cond.NE),
          BCond(asmGlobal ~ "_errBadChar", Cond.NE)
        )
    )

  /** Generate assemply code to calculate a list of expr and push them into the stack. Return a list
    * of code string and an offset indicating the amount of stack space needed
    */
  private def pushArgs(
      funcName: String,
      argList: List[Expr],
      registerMap: RegisterMap,
      scope: Scope
  )(implicit
      symbolTable: FrozenSymbolTable
  ): (StringBuilder, Int) =
    val paramTypes = symbolTable.getFuncSignature(funcName).paramTypes
    var paramCount: Int = 0
    var offset: Int = 0

    join(
      argList
        .zip(paramTypes)
        .map: (expr, paramType) =>
          val paramSize: Int = TypeBridge.fromAst(paramType).byteSize
          val dest = if paramSize > 4 then XRegister(paramCount) else WRegister(paramCount)
          if (paramCount < 8)
            paramCount += 1
            generateExpr(expr, registerMap, scope)
          else
            offset += paramSize
            join(
              generateExpr(expr, registerMap, scope),
              STR(dest, Offset(sp, ImmVal(offset - paramSize)))
            )
      *
    ) -> (offset + 15) / 16 * 16

  /** Generate code to push and pop all registers in `regs` to the stack. The generated code works
    * if only if the stack pointers (sp) after the push and before the pop are the same.
    */
  private def pushAndPopRegisters(
      regs: Seq[Register]
  ): (StringBuilder, StringBuilder) =

    val numReg: Int = regs.size
    val offset = (numReg + 1) / 2 * 16

    val pushComment = Comment(s"push {${regs.mkString(", ")}}")(4)
    val popComment = Comment(s"pop {${regs.mkString(", ")}}")(4)

    if (numReg == 0) (join(), join())
    else if (numReg == 1)
      (
        join(STP(regs(0), xzr, PreIndex(sp, ImmVal(-offset)))),
        join(LDP(regs(0), xzr, PostIndex(sp, ImmVal(offset))))
      )
    else
      val firstPush = STP(regs(0), regs(1), PreIndex(sp, ImmVal(-offset)))
      val lastPop = LDP(regs(0), regs(1), PostIndex(sp, ImmVal(offset)))

      val pairedInstrs = (2 until numReg by 2)
        .map: pushedNum =>
          val r1 = regs(pushedNum)
          val r2 = if (pushedNum + 1 < numReg) regs(pushedNum + 1) else xzr
          (
            STP(r1, r2, Offset(sp, ImmVal(8 * pushedNum))),
            LDP(r1, r2, Offset(sp, ImmVal(8 * pushedNum)))
          )
        .unzip
      (
        join(
          pushComment +:
            firstPush +:
            pairedInstrs._1*
        ),
        join(
          popComment +:
            pairedInstrs._2 :+
            lastPop*
        )
      )

  private def join(codes: AsmSnippet | StringBuilder | String*): StringBuilder =
    StringBuilder().appendAll(codes*)
