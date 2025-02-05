package semanticCheckers

import ast.*

import errors.*
import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable.ListBuffer

object SemanticChecker {
    val defaultPos: (Int, Int) = (-1, -1)
    val anyType: WaccType = AnyType()(defaultPos)

    def weakens(tarT: WaccType, srcT: WaccType): Boolean = (tarT, srcT) match
        case (ArrayType(CharType()), StringType()) => true
        case (StringType(), ArrayType(CharType())) => true
        case _                                     => false

    def compatible(tarT: WaccType, srcT: WaccType): Boolean =
        tarT == srcT || ((tarT, srcT) match {
            case (UnknownType(), _) => true
            case (_, UnknownType()) => true
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

    def commonAncestor(es: List[Expr])(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): WaccType = es.map(getType).distinct match {
        case Nil => ArrayType(anyType)(defaultPos)
        case ts @ (head :: tail) => {
            var resultType = head
            var invalid = false
            breakable {
                tail.foreach { t =>
                    if (compatible(resultType, t)) {
                        resultType = t;
                    } else if (!compatible(t, resultType)) {
                        invalid = true
                        break()
                    }
                }
            }
            if (invalid) {
                errors +=
                    ErrorBuilder.specializedError(
                      Seq(
                        "Type Error: array literal mismatch",
                        s"literal contains mix of ${ts.mkString(", ")}"
                      ),
                      es.head.pos
                    )
                AnyType()(defaultPos)
            } else {
                ArrayType(resultType)(defaultPos)
            }
        }
    }

    def getType(rVal: RValue)(implicit
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
                                Seq(s"Function call error: wrong number of arguments provided to function ${name}"), 
                                id.pos
                            )
                    }
                    es.zip(ts).foreach {
                        case (e, t) =>
                            if (!compatible(t, getType(e))) {
                                errors +=
                                    ErrorBuilder.vanillaError(
                                    s"${t.toString()}",
                                    s"${{getType(e).toString()}}",
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
                          Seq(
                            s"Undefined error: function $name has not been defined"
                          ),
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
                case ErasedPairType()        => UnknownType()(defaultPos)
                case errT @ (FirstErrorType(_) | SecondErrorType(_)) => FirstErrorType(errT)(errT.pos)
                case t => {
                    FirstErrorType(t)(insideLVal.pos)
                }
            }

        case Second(insideLVal) =>
            getType(insideLVal) match {
                case NonErasedPairType(_, t) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
                case errT @ (FirstErrorType(_) | SecondErrorType(_)) => SecondErrorType(errT)(errT.pos)
                case t => SecondErrorType(t)(insideLVal.pos)
            }
    }

    def getType(lVal: LValue)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): WaccType = lVal match {
        case First(insideLVal) =>
            getType(insideLVal) match {
                case NonErasedPairType(t, _) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
                case errT @ (FirstErrorType(_) | SecondErrorType(_)) => FirstErrorType(errT)(errT.pos)
                case t => {
                    FirstErrorType(t)(insideLVal.pos)
                }
            }

        case Second(insideLVal) =>
            getType(insideLVal) match {
                case NonErasedPairType(_, t) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
                case errT @ (FirstErrorType(_) | SecondErrorType(_)) => SecondErrorType(errT)(errT.pos)
                case t => SecondErrorType(t)(insideLVal.pos)
            }
        case e: Expr => getType(e: Expr)
    }

    def getType(expr: Expr)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): WaccType = expr match {
        // Literal cases
        case IntLiter(_)  => IntType()(defaultPos)
        case BoolLiter(_) => BoolType()(defaultPos)
        case CharLiter(_) => CharType()(defaultPos)
        case StrLiter(_)  => StringType()(defaultPos)
        case PairLiter() =>
            NonErasedPairType(anyType, anyType)(
              defaultPos
            )

        // Parentheses
        case Paren(e) => getType(e)

        // Unary operators
        case e @ Not(_) =>
            verifyUnary(e)
            BoolType()(defaultPos)
        case e @ (Negate(_) | Len(_) | Ord(_)) =>
            verifyUnary(e)
            IntType()(defaultPos)
        case e @ Chr(_) =>
            verifyUnary(e)
            CharType()(defaultPos)

        // Binary operators producing Int results
        case e @ (Mul(_, _) | Div(_, _) | Mod(_, _) | Add(_, _) | Sub(_, _)) =>
            verifyBinary(e)
            IntType()(defaultPos)

        // Binary operators producing Bool results
        case e @ (Less(_, _) | LessEqual(_, _) | Greater(_, _) |
            GreaterEqual(_, _) | Equal(_, _) | NotEqual(_, _) | And(_, _) |
            Or(_, _)) =>
            verifyBinary(e)
            BoolType()(defaultPos)

        // Identifiers
        case (id @ Ident(name)) =>
            st.lookupSymbol(name) match {
                case Some(t) => t
                case None => {
                    errors +=
                        ErrorBuilder.specializedError(
                          Seq(
                            s"Scope error: variable $name has not been declared in this scope"
                          ),
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
                        verifyType(head, IntType()(defaultPos)) 
                        getArrayElemType(t, tail)
                    case (t, _) => {
                        errors +=
                            ErrorBuilder.specializedError(
                                Seq(
                                    s"index error: bad indexing on variable ${id.name} of type ${t}"
                                ),
                                id.pos
                            )
                        anyType
                    }
                }
            }
            val t: WaccType = getType(id: Expr)
            getArrayElemType(t, es)

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

    private def verifyType(e: LValue, expT: WaccType*)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT, e.pos)

    private def verifyType(e: RValue, expT: WaccType*)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT, e.pos)

    private def verifyType(e: Expr, expT: WaccType*)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT, e.pos)

    def verifyUnary(expr: UnaryOp)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit = expr match {
        case Not(e)    => verifyType(e, BoolType()(defaultPos))
        case Negate(e) => verifyType(e, IntType()(defaultPos))
        case Len(e)    => verifyType(e, ArrayType(anyType)(defaultPos))
        case Ord(e)    => verifyType(e, CharType()(defaultPos))
        case Chr(e)    => verifyType(e, IntType()(defaultPos))
    }

    def verifyBinary(expr: BinaryOp)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit = expr match {
        case Mul(e1, e2) =>
            verifyType(e1, IntType()(defaultPos))
            verifyType(e2, IntType()(defaultPos))
        case Div(e1, e2) =>
            verifyType(e1, IntType()(defaultPos))
            verifyType(e2, IntType()(defaultPos))
        case Mod(e1, e2) =>
            verifyType(e1, IntType()(defaultPos))
            verifyType(e2, IntType()(defaultPos))
        case Add(e1, e2) =>
            verifyType(e1, IntType()(defaultPos))
            verifyType(e2, IntType()(defaultPos))
        case Sub(e1, e2) =>
            verifyType(e1, IntType()(defaultPos))
            verifyType(e2, IntType()(defaultPos))
        case Greater(e1, e2) =>
            verifyType(e1, IntType()(defaultPos), CharType()(defaultPos))
            verifyType(e2, IntType()(defaultPos), CharType()(defaultPos))
        case GreaterEqual(e1, e2) =>
            verifyType(e1, IntType()(defaultPos), CharType()(defaultPos))
            verifyType(e2, IntType()(defaultPos), CharType()(defaultPos))
        case Less(e1, e2) =>
            verifyType(e1, IntType()(defaultPos), CharType()(defaultPos))
            verifyType(e2, IntType()(defaultPos), CharType()(defaultPos))
        case LessEqual(e1, e2) =>
            verifyType(e1, IntType()(defaultPos), CharType()(defaultPos))
            verifyType(e2, IntType()(defaultPos), CharType()(defaultPos))
        case Equal(e1, e2) =>
            verifyType(e2, getType(e1))
        case NotEqual(e1, e2) =>
            verifyType(e2, getType(e1))
        case And(e1, e2) =>
            verifyType(e1, BoolType()(defaultPos))
            verifyType(e2, BoolType()(defaultPos))
        case Or(e1, e2) =>
            verifyType(e1, BoolType()(defaultPos))
            verifyType(e2, BoolType()(defaultPos))
    }

    def errorTypePrettyPrint(t: WaccType, innerType: WaccType): (String, String) = t match {
        case FirstErrorType(inner) => inner match {
            // If inner is itself an error type, recurse with flipped = true
            case _: FirstErrorType | _: SecondErrorType =>
                val (base, innerExpected) = errorTypePrettyPrint(inner, innerType)
                val exp = s"pair($innerExpected, any type)"
                (base, exp)
            case _ =>
            // inner is a “normal” type
                val base = inner.toString
                val exp = s"pair(${innerType.toString}, any type)" 
                (base, exp)
        }
        case SecondErrorType(inner) => inner match {
            case _: FirstErrorType | _: SecondErrorType =>
                val (base, innerExpected) = errorTypePrettyPrint(inner, innerType)
                val exp = s"pair(any type, $innerExpected)"
                (base, exp)
            case _ =>
                val base = inner.toString
                val exp = s"pair(any type, ${innerType.toString})" 
                (base, exp)
        }
        case other =>
            // For non-error types, just use toString
            (other.toString, other.toString)
        }


    def verifyStmt(stmt: Stmt)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit = stmt match {
        case Exit(e) => verifyType(e, IntType()(defaultPos))
        case If(e, s1, s2) =>
            verifyType(e, BoolType()(defaultPos))
            verifyStmt(s1)
            verifyStmt(s2)
        case While(e, s) =>
            verifyType(e, BoolType()(defaultPos))
            verifyStmt(s)
        case Print(e)   => verifyType(e, anyType)
        case Println(e) => verifyType(e, anyType)
        case Read(e)    => getType(e) match {
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
                val (unMsg, msgChar) = errorTypePrettyPrint(errT, CharType()(defaultPos))
                val (_, msgInt) = errorTypePrettyPrint(errT, IntType()(defaultPos))
                errors +=
                    ErrorBuilder.vanillaError(
                        unMsg, 
                        s"$msgChar or $msgInt", 
                        Seq(), 
                        errT.pos
                    )
            case _ => verifyType(e, IntType()(defaultPos), CharType()(defaultPos))
        }
        case Declare((t, Ident(name)), v) => {
            if (!st.addSymbol(name, t)) {
                errors +=
                    ErrorBuilder.specializedError(
                      Seq(
                        s"Scope error: illegal redeclaration of variable $name "
                      ),
                      stmt.pos
                    )
            }
            getType(v) match {
                case errT @ (FirstErrorType(_) | SecondErrorType(_)) => 
                    val (unMsg, msg) = errorTypePrettyPrint(errT, t)
                    errors +=
                        ErrorBuilder.vanillaError(
                            unMsg, 
                            msg,
                            Seq(), 
                            errT.pos
                        )
                case _ => verifyType(v, t)
            }
        }
        case Assign(v1, v2) =>
            (getType(v1), getType(v2)) match {
                case (UnknownType(), UnknownType()) =>
                    errors +=
                        ErrorBuilder.specializedError(
                          Seq(
                            "Type error: attempting to exchange values between pairs of unknown types",
                            "pair exchange is only legal when the type of at least one of the sides is known or specified"
                          ),
                          stmt.pos
                        )
                case (
                    FirstErrorType(_) | SecondErrorType(_), 
                    FirstErrorType(_) | SecondErrorType(_)
                ) => 
                    // TODO: This is different from the references
                    errors +=
                        ErrorBuilder.specializedError(
                          Seq(
                            "Type error: attempting to exchange values between pairs of unknown types",
                            "pair exchange is only legal when the type of at least one of the sides is known or specified"
                          ),
                          stmt.pos
                        )
                case (errT @ (FirstErrorType(_) | SecondErrorType(_)), t) => 
                    val (unMsg, msg) = errorTypePrettyPrint(errT, t)
                    errors +=
                        ErrorBuilder.vanillaError(
                            unMsg, 
                            msg,
                            Seq(), 
                            errT.pos
                        )
                case (t, errT @ (FirstErrorType(_) | SecondErrorType(_))) => 
                    val (unMsg, msg) = errorTypePrettyPrint(errT, t)
                    errors +=
                        ErrorBuilder.vanillaError(
                            unMsg, 
                            msg,
                            Seq(), 
                            errT.pos
                        )
                case (t1, _) => verifyType(v2, t1)
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
                      Seq(
                        "Return placement error: return outside of function is not allowed"
                      ),
                      stmt.pos
                    )
    }
}
