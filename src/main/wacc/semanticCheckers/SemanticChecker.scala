package semanticCheckers

import ast.*
import errors.*
import scala.collection.mutable.ListBuffer

object SemanticChecker {
  val defaultPos: (Int, Int) = (-1, -1)
  val anyType: WaccType = AnyType()(defaultPos)
  val intType: WaccType = IntType()(defaultPos)
  val boolType: WaccType = BoolType()(defaultPos)
  val charType: WaccType = CharType()(defaultPos)
  val stringType: WaccType = StringType()(defaultPos)
  val unknownType: WaccType = UnknownType()(defaultPos)

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
  ): WaccType = es.map(getType).distinct match {
    case Nil => ArrayType(anyType)(defaultPos)
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
        ArrayType(resultType)(defaultPos)
      }
  }

  private def getType(expr: Expr)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): WaccType = expr match {
    // Literal cases
    case IntLiter(_)  => intType
    case BoolLiter(_) => boolType
    case CharLiter(_) => charType
    case StrLiter(_)  => stringType
    case PairLiter() =>
      NonErasedPairType(anyType, anyType)(
        defaultPos
      )

    // Parentheses
    case Paren(e) => getType(e)

    // Unary operators
    case e @ Not(_) =>
      verifyUnary(e)
      boolType
    case e @ (Negate(_) | Len(_) | Ord(_)) =>
      verifyUnary(e)
      intType
    case e @ Chr(_) =>
      verifyUnary(e)
      charType

    // Binary operators producing Int results
    case e @ (Mul(_, _) | Div(_, _) | Mod(_, _) | Add(_, _) | Sub(_, _)) =>
      verifyBinary(e)
      intType

    // Binary operators producing Bool results
    case e @ (Less(_, _) | LessEqual(_, _) | Greater(_, _) | GreaterEqual(_, _) | Equal(_, _) |
        NotEqual(_, _) | And(_, _) | Or(_, _)) =>
      verifyBinary(e)
      boolType

    // Identifiers
    case (id @ Ident(name)) =>
      st.lookupSymbol(name) match {
        case Some(t) => t
        case None => {
          errors +=
            ErrorBuilder.specializedError(
              Seq(s"Scope error: variable $name has not been declared in this scope"),
              id.pos
            )
          anyType
        }
      }

    // Array elements
    case ArrayElem(id, es) =>
      def getArrayElemType(t: WaccType, exprs: List[Expr]): WaccType = {
        (t, exprs) match {
          case (t, Nil) => t
          case (ArrayType(t), head :: tail) =>
            verifyType(head, intType)
            getArrayElemType(t, tail)
          case (t, _) => {
            errors +=
              ErrorBuilder.specializedError(
                Seq(s"index error: bad indexing on variable ${id.name} of type ${t}"),
                id.pos
              )
            anyType
          }
        }
      }
      val t: WaccType = getType(id: Expr)
      getArrayElemType(t, es)
  }

  private def getType(rVal: RValue)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): WaccType = rVal match {
    case ArrayLiter(es) =>
      commonAncestor(es)

    case NewPair(e1, e2) => {
      val t1 = getType(e1)
      val t2 = getType(e2)
      NonErasedPairType(t1, t2)(defaultPos)
    }

    case Call(id @ Ident(name), ArgList(es)) => {

      st.lookupFunction(name) match {
        case Some(FunctionSignature(rt, ts)) => {
          if (es.size != ts.size) {
            errors +=
              ErrorBuilder.vanillaError(
                s"${es.size} arguments",
                s"${ts.size} arguments",
                Seq(
                  s"Function call error: wrong number of arguments provided to function ${name}"
                ),
                id.pos
              )
          }
          es.zip(ts).foreach { case (e, t) =>
            if (!compatible(t, getType(e))) {
              errors +=
                ErrorBuilder.vanillaError(
                  s"${t.toString()}",
                  s"${{ getType(e).toString() }}",
                  Seq(),
                  e.pos
                )
            }
          }
          rt
        }
        case None => {
          errors +=
            ErrorBuilder.specializedError(
              Seq(s"Undefined error: function $name has not been defined"),
              id.pos
            )
          anyType
        }
      }
    }

    case e: Expr => getType(e)

    case First(insideLVal) =>
      getType(insideLVal) match {
        case NonErasedPairType(t, _) => t
        case ErasedPairType()        => unknownType
        case errT @ (FirstErrorType(_) | SecondErrorType(_)) =>
          FirstErrorType(errT)(errT.pos)
        case t => FirstErrorType(t)(insideLVal.pos)
      }

    case Second(insideLVal) =>
      getType(insideLVal) match {
        case NonErasedPairType(_, t) => t
        case ErasedPairType()        => unknownType
        case errT @ (FirstErrorType(_) | SecondErrorType(_)) =>
          SecondErrorType(errT)(errT.pos)
        case t => SecondErrorType(t)(insideLVal.pos)
      }
  }

  private def getType(lVal: LValue)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): WaccType = lVal match {
    case v: RValue => getType(v: RValue)
  }

  private def verifyTypeHelper(
      t: WaccType,
      expT: Seq[WaccType],
      pos: (Int, Int)
  )(implicit
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): Unit =
    if (expT.forall(!compatible(t, _)))
      errors +=
        ErrorBuilder.vanillaError(
          s"${t.toString()}",
          expT.mkString(", "),
          Seq(),
          pos
        )

  private def verifyType(e: Expr, expT: WaccType*)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): Unit = verifyTypeHelper(getType(e), expT, e.pos)

  private def verifyUnary(expr: UnaryOp)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): Unit = expr match {
    case Not(e)    => verifyType(e, boolType)
    case Negate(e) => verifyType(e, intType)
    case Len(e)    => verifyType(e, ArrayType(anyType)(defaultPos))
    case Ord(e)    => verifyType(e, charType)
    case Chr(e)    => verifyType(e, intType)
  }

  private def verifyBinary(expr: BinaryOp)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): Unit = expr match {
    case Mul(e1, e2) =>
      verifyType(e1, intType)
      verifyType(e2, intType)
    case Div(e1, e2) =>
      verifyType(e1, intType)
      verifyType(e2, intType)
    case Mod(e1, e2) =>
      verifyType(e1, intType)
      verifyType(e2, intType)
    case Add(e1, e2) =>
      verifyType(e1, intType)
      verifyType(e2, intType)
    case Sub(e1, e2) =>
      verifyType(e1, intType)
      verifyType(e2, intType)
    case Greater(e1, e2) =>
      verifyType(e1, intType, charType)
      verifyType(e2, intType, charType)
    case GreaterEqual(e1, e2) =>
      verifyType(e1, intType, charType)
      verifyType(e2, intType, charType)
    case Less(e1, e2) =>
      verifyType(e1, intType, charType)
      verifyType(e2, intType, charType)
    case LessEqual(e1, e2) =>
      verifyType(e1, intType, charType)
      verifyType(e2, intType, charType)
    case Equal(e1, e2) =>
      verifyType(e2, getType(e1))
    case NotEqual(e1, e2) =>
      verifyType(e2, getType(e1))
    case And(e1, e2) =>
      verifyType(e1, boolType)
      verifyType(e2, boolType)
    case Or(e1, e2) =>
      verifyType(e1, boolType)
      verifyType(e2, boolType)
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
  ): Unit = stmt match {
    case Exit(e) => verifyType(e, intType)
    case If(e, s1, s2) =>
      verifyType(e, boolType)
      verifyStmt(s1)
      verifyStmt(s2)
    case While(e, s) =>
      verifyType(e, boolType)
      verifyStmt(s)
    case Print(e)   => verifyType(e, anyType)
    case Println(e) => verifyType(e, anyType)
    case Read(e) =>
      getType(e) match {
        case UnknownType() =>
          errors +=
            ErrorBuilder.specializedError(
              Seq(
                "Type error: attempting to read from unknown type",
                "reading from a nested pair extraction is not legal due to pair erasure"
              ),
              stmt.pos
            )
        case errT @ (FirstErrorType(_) | SecondErrorType(_)) =>
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
        case t =>
          if (!compatible(t, intType) && !compatible(t, charType))
            errors +=
              ErrorBuilder.vanillaError(
                t.toString(),
                s"int or char",
                Seq(),
                stmt.pos
              )
      }
    case Declare((t, Ident(name)), v) => {
      if (!st.addSymbol(name, t)) {
        errors +=
          ErrorBuilder.specializedError(
            Seq(s"Scope error: illegal redeclaration of variable $name "),
            stmt.pos
          )
      }
      getType(v) match {
        case errT @ (FirstErrorType(_) | SecondErrorType(_)) =>
          val (unMsg, msg) = errorTypePrettyPrint(errT, t)
          errors +=
            ErrorBuilder.vanillaError(unMsg, msg, Seq(), errT.pos)
        case vt =>
          if (!compatible(vt, t))
            errors +=
              ErrorBuilder.vanillaError(
                vt.toString(),
                t.toString,
                Seq(),
                stmt.pos
              )
      }
    }
    case Assign(v1, v2) =>
      (getType(v1), getType(v2)) match {
        case (
              FirstErrorType(_) | SecondErrorType(_),
              FirstErrorType(_) | SecondErrorType(_)
            ) | (UnknownType(), UnknownType()) =>
          errors +=
            ErrorBuilder.specializedError(
              Seq(
                "Type error: attempting to exchange values between pairs of unknown types",
                "pair exchange is only legal when the type of at least one of " +
                  "the sides is known or specified"
              ),
              stmt.pos
            )
        case (errT @ (FirstErrorType(_) | SecondErrorType(_)), t) =>
          val (unMsg, msg) = errorTypePrettyPrint(errT, t)
          errors +=
            ErrorBuilder.vanillaError(unMsg, msg, Seq(), errT.pos)
        case (t, errT @ (FirstErrorType(_) | SecondErrorType(_))) =>
          val (unMsg, msg) = errorTypePrettyPrint(errT, t)
          errors +=
            ErrorBuilder.vanillaError(unMsg, msg, Seq(), errT.pos)
        case (t1, t2) =>
          if (!compatible(t1, t2))
            errors +=
              ErrorBuilder.vanillaError(
                t2.toString(),
                t1.toString,
                Seq(),
                stmt.pos
              )
      }
    case Begin(stmt) => verifyStmt(stmt)
    case Block(sts) =>
      st.enterScope()
      sts.foreach(verifyStmt)
      st.exitScope()
    case Skip() =>
    case Free(e) =>
      verifyType(
        e,
        ArrayType(anyType)(defaultPos),
        NonErasedPairType(anyType, anyType)(defaultPos)
      )
    case Return(e) =>
      verifyType(e, st.getReturnType())
      if (st.isGlobalScope())
        errors +=
          ErrorBuilder.specializedError(
            Seq("Return placement error: return outside of function is not allowed"),
            stmt.pos
          )
  }
}
