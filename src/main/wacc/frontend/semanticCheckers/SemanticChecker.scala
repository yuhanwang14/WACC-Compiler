package frontend.semanticCheckers

import frontend.ast.*
import errors.*
import scala.collection.mutable.ListBuffer
import common.SymbolTable

object SemanticChecker {

  private def weakens(tarT: WaccType, srcT: WaccType): Boolean = (tarT, srcT) match
    case (ArrayType(CharType()), StringType()) => true
    case (StringType(), ArrayType(CharType())) => true
    case _                                     => false

  private def compatible(tarT: WaccType, srcT: WaccType): Boolean =
    tarT == srcT || ((tarT, srcT) match {
      case (UnknownType(), _)                    => true
      case (_, UnknownType())                    => true
      case (ArrayType(CharType()), StringType()) => true
      case (ArrayType(tarElemT), ArrayType(srcElemT)) =>
        compatible(tarElemT, srcElemT) && !weakens(tarElemT, srcElemT)
      case (
            NonErasedPairType(tarElemT1, tarElemT2),
            NonErasedPairType(srcElemT1, srcElemT2)
          ) =>
        compatible(tarElemT1, srcElemT1) && compatible(
          tarElemT2,
          srcElemT2
        ) &&
        !weakens(tarElemT1, srcElemT1) && !weakens(tarElemT2, srcElemT2)
      case (NonErasedPairType(_, _), ErasedPairType()) => true
      case (ErasedPairType(), NonErasedPairType(_, _)) => true
      case (AnyType(), _)                              => true
      case (_, AnyType())                              => true
      case _                                           => false
    })

  private def commonAncestor(es: List[Expr])(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): (WaccType, List[Expr]) =
    val tes = es.map(getType)
    (tes.map(_._1).distinct match
      case Nil => anyType
      case ts @ (head :: tail) =>
        val (resultType, invalid) = tail.foldLeft((head, false)) {
          case ((currentType, isInvalid), t) =>
            if (isInvalid) (currentType, true)
            else if (compatible(currentType, t)) (t, false)
            else if (compatible(t, currentType)) (currentType, false)
            else (currentType, true)
        }
        if (invalid) {
          errors += ErrorBuilder.specializedError(
            Seq(
              "Type Error: array literal mismatch",
              s"literal contains mix of ${ts.mkString(", ")}"
            ),
            es.head.pos
          )
          anyType
        } else {
          resultType
        }
    )
      -> tes.map(_._2)

  private def getType(expr: Expr)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): (WaccType, Expr) = expr match {
    // Literal cases
    case IntLiter(_)  => intType -> expr
    case BoolLiter(_) => boolType -> expr
    case CharLiter(_) => charType -> expr
    case StrLiter(_)  => stringType -> expr
    case PairLiter() =>
      NonErasedPairType(anyType, anyType)(
        defaultPos
      ) -> expr
    case Paren(e) => getType(e)

    // Unary operators
    case e @ Not(_) =>
      boolType -> verifyUnary(e)
    case e @ (Negate(_) | Len(_) | Ord(_)) =>
      intType -> verifyUnary(e)
    case e @ Chr(_) =>
      charType -> verifyUnary(e)

    // Binary operators producing Int results
    case e @ (Mul(_, _) | Div(_, _) | Mod(_, _) | Add(_, _) | Sub(_, _)) =>
      intType -> verifyBinary(e)

    // Binary operators producing Bool results
    case e @ (Less(_, _) | LessEqual(_, _) | Greater(_, _) | GreaterEqual(_, _) | Equal(_, _) |
        NotEqual(_, _) | And(_, _) | Or(_, _)) =>
      boolType -> verifyBinary(e)

    // Identifiers
    case (id @ Ident(name)) =>
      st.lookupSymbol(name) match {
        case Some(t) => (t, Ident(st.unshadow(name).get)(id.pos))
        case None => {
          errors +=
            ErrorBuilder.specializedError(
              Seq(s"Scope error: variable $name has not been declared in this scope"),
              id.pos
            )
          anyType -> id
        }
      }

    // Array elements
    case ArrayElem(id, es) =>
      def getArrayElemType(t: WaccType, exprs: List[Expr]): (WaccType, List[Expr]) =
        (t, exprs) match
          case (t, Nil) => (t, List())
          case (ArrayType(t), head :: tail) =>
            val e = verifyType(head, intType)._2
            val (elemT, rem) = getArrayElemType(t, tail)
            (elemT, e +: rem)
          case (t, _) =>
            errors +=
              ErrorBuilder.specializedError(
                Seq(s"index error: bad indexing on variable ${id.name} of type ${t}"),
                id.pos
              )
            (anyType, exprs)
      val (t, newId) = getType(id: Expr)
      val (arrayElemType, newEs) = getArrayElemType(t, es)
      arrayElemType
        -> ArrayElem(
          (newId match
            case i: Ident => i
            case _        => id
          ),
          newEs
        )(expr.pos)
  }

  private def getType(rVal: RValue)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): (WaccType, RValue) = rVal match {
    case ArrayLiter(es) =>
      commonAncestor(es) match
        case (t @ BoolType(), newEs)   => (ArrayType(t)(defaultPos), ArrayLiterB(newEs)(rVal.pos))
        case (t @ CharType(), newEs)   => (ArrayType(t)(defaultPos), ArrayLiterC(newEs)(rVal.pos))
        case (t @ IntType(), newEs)    => (ArrayType(t)(defaultPos), ArrayLiterI(newEs)(rVal.pos))
        case (t @ StringType(), newEs) => (ArrayType(t)(defaultPos), ArrayLiterS(newEs)(rVal.pos))
        case (t @ ArrayType(_), newEs) => (ArrayType(t)(defaultPos), ArrayLiterP(newEs)(rVal.pos))
        case (t @ NonErasedPairType(_, _), newEs) =>
          (ArrayType(t)(defaultPos), ArrayLiterP(newEs)(rVal.pos))
        case (t, newEs) => (ArrayType(t)(defaultPos), ArrayLiter(newEs)(rVal.pos))

    // case Print(e) =>
    //   verifyType(e, anyType) match

    case NewPair(e1, e2) => {
      val (t1, newE1) = getType(e1)
      val (t2, newE2) = getType(e2)
      NonErasedPairType(t1, t2)(defaultPos) -> NewPair(newE1, newE2)(rVal.pos)
    }

    case Call(id @ Ident(name), args @ ArgList(es)) => {
      st.lookupFunction(name) match {
        case Some(f) => {
          if (es.size != f.paramTypes.size) {
            errors +=
              ErrorBuilder.vanillaError(
                s"${es.size} arguments",
                s"${f.paramTypes.size} arguments",
                Seq(
                  s"Function call error: wrong number of arguments provided to function ${name}"
                ),
                id.pos
              )
          }
          val newEs = es.zip(f.paramTypes).map { (e, expT) =>
            val (t, newE) = getType(e)
            if (!compatible(expT, t)) {
              errors +=
                ErrorBuilder.vanillaError(
                  s"${t.toString()}",
                  s"${{ getType(e).toString() }}",
                  Seq(),
                  e.pos
                )
            }
            newE
          }
          f.returnType -> Call(id, ArgList(newEs)(args.pos))(rVal.pos)
        }
        case None => {
          errors +=
            ErrorBuilder.specializedError(
              Seq(s"Undefined error: function $name has not been defined"),
              id.pos
            )
          anyType -> rVal
        }
      }
    }

    case e: Expr => getType(e)

    case First(insideLVal) =>
      val (t, newE) = getType(insideLVal)
      (t match
        case NonErasedPairType(t, _) => t
        case ErasedPairType()        => unknownType
        case errT @ (FirstErrorType(_) | SecondErrorType(_)) =>
          FirstErrorType(errT)(errT.pos)
        case t => FirstErrorType(t)(insideLVal.pos)
      )
        -> First(newE)(rVal.pos)

    case Second(insideLVal) =>
      val (t, newE) = getType(insideLVal)
      (t match
        case NonErasedPairType(_, t) => t
        case ErasedPairType()        => unknownType
        case errT @ (FirstErrorType(_) | SecondErrorType(_)) =>
          SecondErrorType(errT)(errT.pos)
        case t => SecondErrorType(t)(insideLVal.pos)
      )
        -> Second(newE)(rVal.pos)
  }

  private def getType(lVal: LValue)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): (WaccType, LValue) = lVal match {
    case v: RValue =>
      getType(v: RValue) match
        case (t, newE: LValue) => t -> newE
        case (t, _)            => throw Exception()
  }

  private def verifyTypeHelper(t: WaccType, expT: Iterable[WaccType])(pos: (Int, Int))(implicit
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ) =
    if (expT.forall(!compatible(t, _)))
      errors +=
        ErrorBuilder.vanillaError(
          s"${t.toString()}",
          expT.mkString(", "),
          Seq(),
          pos
        )

  private def verifyType(expr: Expr, expT: WaccType*)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): (WaccType, Expr) =
    val (t, newE) = getType(expr)
    verifyTypeHelper(t, expT)(expr.pos)
    (t, newE)

  private def verifyType(expr: LValue, expT: WaccType*)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): (WaccType, LValue) =
    val (t, newE) = getType(expr)
    verifyTypeHelper(t, expT)(expr.pos)
    (t, newE)

  private def verifyUnary(expr: UnaryOp)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): Expr = expr match {
    case Not(e)    => Not(verifyType(e, boolType)._2)(expr.pos)
    case Negate(e) => Negate(verifyType(e, intType)._2)(expr.pos)
    case Len(e)    => Len(verifyType(e, arrayType)._2)(expr.pos)
    case Ord(e)    => Ord(verifyType(e, charType)._2)(expr.pos)
    case Chr(e)    => Chr(verifyType(e, intType)._2)(expr.pos)
  }

  private def verifyBinary(expr: BinaryOp)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): Expr = expr match {
    case Mul(e1, e2) =>
      Mul(verifyType(e1, intType)._2, verifyType(e2, intType)._2)(expr.pos)
    case Div(e1, e2) =>
      Div(verifyType(e1, intType)._2, verifyType(e2, intType)._2)(expr.pos)
    case Mod(e1, e2) =>
      Mod(verifyType(e1, intType)._2, verifyType(e2, intType)._2)(expr.pos)
    case Add(e1, e2) =>
      Add(verifyType(e1, intType)._2, verifyType(e2, intType)._2)(expr.pos)
    case Sub(e1, e2) =>
      Sub(verifyType(e1, intType)._2, verifyType(e2, intType)._2)(expr.pos)
    case Greater(e1, e2) =>
      Greater(verifyType(e1, intType, charType)._2, verifyType(e2, intType, charType)._2)(expr.pos)
    case GreaterEqual(e1, e2) =>
      GreaterEqual(verifyType(e1, intType, charType)._2, verifyType(e2, intType, charType)._2)(
        expr.pos
      )
    case Less(e1, e2) =>
      Less(verifyType(e1, intType, charType)._2, verifyType(e2, intType, charType)._2)(expr.pos)
    case LessEqual(e1, e2) =>
      LessEqual(verifyType(e1, intType, charType)._2, verifyType(e2, intType, charType)._2)(
        expr.pos
      )
    case Equal(e1, e2) =>
      val (t1, newE1) = getType(e1)
      Equal(newE1, verifyType(e2, t1)._2)(expr.pos)
    case NotEqual(e1, e2) =>
      val (t1, newE1) = getType(e1)
      NotEqual(newE1, verifyType(e2, t1)._2)(expr.pos)
    case And(e1, e2) =>
      And(verifyType(e1, boolType)._2, verifyType(e2, boolType)._2)(expr.pos)
    case Or(e1, e2) =>
      Or(verifyType(e1, boolType)._2, verifyType(e2, boolType)._2)(expr.pos)
  }

  private def errorTypePrettyPrint(t: WaccType, innerType: WaccType): (String, String) = t match {
    case FirstErrorType(inner)  => formatErrorType(inner, innerType, first = true)
    case SecondErrorType(inner) => formatErrorType(inner, innerType, first = false)
    case other                  => (other.toString, other.toString)
  }

  private def formatErrorType(
      inner: WaccType,
      innerType: WaccType,
      first: Boolean
  ): (String, String) = inner match {
    case _: FirstErrorType | _: SecondErrorType =>
      val (base, innerExpected) = errorTypePrettyPrint(inner, innerType)
      val expected =
        if (first) s"pair($innerExpected, any type)" else s"pair(any type, $innerExpected)"
      (base, expected)
    case _ =>
      val base = inner.toString
      val expected =
        if (first) s"pair(${innerType.toString}, any type)"
        else s"pair(any type, ${innerType.toString})"
      (base, expected)
  }

  def verifyStmt(stmt: Stmt)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): Stmt = stmt match
    case Exit(e)       => Exit(verifyType(e, intType)._2)(stmt.pos)
    case If(e, s1, s2) => If(verifyType(e, boolType)._2, verifyStmt(s2), verifyStmt(s1))(stmt.pos)
    case While(e, s)   => While(verifyType(e, boolType)._2, verifyStmt(s))(stmt.pos)
    case Print(e) =>
      verifyType(e, anyType) match
        case (BoolType(), newE)              => PrintB(newE)(stmt.pos)
        case (CharType(), newE)              => PrintC(newE)(stmt.pos)
        case (IntType(), newE)               => PrintI(newE)(stmt.pos)
        case (StringType(), newE)            => PrintS(newE)(stmt.pos)
        case (ArrayType(_), newE)            => PrintP(newE)(stmt.pos)
        case (NonErasedPairType(_, _), newE) => PrintP(newE)(stmt.pos)
        case _                               => stmt

    case Println(e) =>
      verifyType(e, anyType) match
        case (BoolType(), newE)              => PrintlnB(newE)(stmt.pos)
        case (CharType(), newE)              => PrintlnC(newE)(stmt.pos)
        case (IntType(), newE)               => PrintlnI(newE)(stmt.pos)
        case (StringType(), newE)            => PrintlnS(newE)(stmt.pos)
        case (ArrayType(_), newE)            => PrintlnP(newE)(stmt.pos)
        case (NonErasedPairType(_, _), newE) => PrintlnP(newE)(stmt.pos)
        case _                               => stmt
    case Read(e) =>
      getType(e) match {
        case (UnknownType(), _) =>
          errors +=
            ErrorBuilder.specializedError(
              Seq(
                "Type error: attempting to read from unknown type",
                "reading from a nested pair extraction is not legal due to pair erasure"
              ),
              stmt.pos
            )
          stmt
        case (errT @ (FirstErrorType(_) | SecondErrorType(_)), _) =>
          val (unMsg, msgChar) =
            errorTypePrettyPrint(errT, charType)
          val (_, msgInt) =
            errorTypePrettyPrint(errT, intType)
          errors +=
            ErrorBuilder.vanillaError(
              unMsg,
              s"$msgChar or $msgInt",
              Seq(),
              errT.pos
            )
          stmt
        case _ =>
          verifyType(e, intType, charType) match
            case (CharType(), newE) => ReadC(newE)(stmt.pos)
            case (IntType(), newE)  => ReadI(newE)(stmt.pos)
            case _                  => stmt
      }
    case Declare((t, id @ Ident(name)), e) =>
      val (vt, newE) = getType(e)
      vt match
        case errT @ (FirstErrorType(_) | SecondErrorType(_)) =>
          val (unMsg, msg) = errorTypePrettyPrint(errT, t)
          errors +=
            ErrorBuilder.vanillaError(unMsg, msg, Seq(), errT.pos)
        case _ =>
          if (!compatible(vt, t))
            errors +=
              ErrorBuilder.vanillaError(
                vt.toString(),
                t.toString,
                Seq(),
                stmt.pos
              )
      if (!st.addSymbol(name, t)) {
        errors +=
          ErrorBuilder.specializedError(
            Seq(s"Scope error: illegal redeclaration of variable $name "),
            stmt.pos
          )
      }
      Declare((t, id.renamed(st.unshadow(name).get)), newE)(stmt.pos)
    case Assign(e1, e2) =>
      (getType(e1), getType(e2)) match
        case (
              (FirstErrorType(_) | SecondErrorType(_)) -> _,
              (FirstErrorType(_) | SecondErrorType(_)) -> _
            ) | (UnknownType() -> _, UnknownType() -> _) =>
          errors +=
            ErrorBuilder.specializedError(
              Seq(
                "Type error: attempting to exchange values between pairs of unknown types",
                "pair exchange is only legal when the type of at least one of " +
                  "the sides is known or specified"
              ),
              stmt.pos
            )
          stmt
        case ((errT @ (FirstErrorType(_) | SecondErrorType(_))) -> _, t -> _) =>
          val (unMsg, msg) = errorTypePrettyPrint(errT, t)
          errors +=
            ErrorBuilder.vanillaError(unMsg, msg, Seq(), errT.pos)
          stmt
        case (t -> _, (errT @ (FirstErrorType(_) | SecondErrorType(_))) -> _) =>
          val (unMsg, msg) = errorTypePrettyPrint(errT, t)
          errors +=
            ErrorBuilder.vanillaError(unMsg, msg, Seq(), errT.pos)
          stmt
        case (t1 -> newE1, t2 -> newE2) =>
          if (!compatible(t1, t2))
            errors +=
              ErrorBuilder.vanillaError(
                t2.toString(),
                t1.toString,
                Seq(),
                stmt.pos
              )
          Assign(newE1, newE2)(stmt.pos)
    case Begin(stmt) => Begin(verifyStmt(stmt))(stmt.pos)
    case Block(sts) =>
      st.enterScope
      val newSts = sts.map(verifyStmt)
      st.exitScope
      Block(newSts)(stmt.pos)
    case Skip() => stmt
    case Free(e) =>
      verifyType(
        e,
        arrayType,
        NonErasedPairType(anyType, anyType)(defaultPos)
      ) match
        case ArrayType(_) -> newE            => FreeA(newE)(stmt.pos)
        case NonErasedPairType(_, _) -> newE => FreeP(newE)(stmt.pos)
        case _                               => stmt
    case Return(e) =>
      st.returnType match
        case None =>
          errors +=
            ErrorBuilder.specializedError(
              Seq("Return placement error: return outside of function is not allowed"),
              stmt.pos
            )
          stmt
        case Some(returnType) =>
          val(_, expr) = verifyType(e, returnType)
          Return(expr)(stmt.pos)

    case _ => stmt
}
