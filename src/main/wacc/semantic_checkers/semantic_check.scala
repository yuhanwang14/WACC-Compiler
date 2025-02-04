package semantic_checkers

import ast.types.*
import ast.statements.*
import ast.expressions.*
import errors.errors.*
import errors.generator.*
import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable.ListBuffer

object semanticChecker {
    val defaultPos: (Int, Int) = (-1, -1)
    val anyType: WACCType = AnyType()(defaultPos)

    def weakens(tarT: WACCType, srcT: WACCType): Boolean = (tarT, srcT) match
        case (ArrayType(CharType()), StringType()) => true
        case (StringType(), ArrayType(CharType())) => true
        case _ => false

    def compatible(tarT: WACCType, srcT: WACCType): Boolean =
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
                compatible(tarElemT1, srcElemT1) && compatible(tarElemT2, srcElemT2) &&
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
    ): WACCType = es.map(getType).distinct match {
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
                    genSpecializedError(
                      Seq(
                        "Type Error: array literal mismatch",
                        s"literal contains mix of ${ts.mkString(",")}"
                      ),
                      es.head.pos
                    )
                NotExistType()(defaultPos)
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
    ): WACCType = rVal match {
        case ArrayLiter(es) =>
            commonAncestor(es)

        case NewPair(e1, e2) => {
            val t1 = getType(e1)
            val t2 = getType(e2)
            NonErasedPairType(t1, t2)(defaultPos)
        }

        case Call(id @ Ident(name), _) => {

            st.lookupFunction(name) match {
                case Some(FunctionSignature(t, _)) => t
                case _ => {
                    errors +=
                        genSpecializedError(
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
                case t => {
                    FirstErrorType(t)(defaultPos)
                }
            }

        case Second(insideLVal) =>
            getType(insideLVal) match {
                case NonErasedPairType(_, t) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
                case t => {
                    SecondErrorType(t)(defaultPos)
                }
            }
    }

    def getType(lVal: LValue)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): WACCType = lVal match {
        case First(insideLVal) =>
            getType(insideLVal) match {
                case NonErasedPairType(t, _) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
                case t => {
                    FirstErrorType(t)(defaultPos)
                }
            }

        case Second(insideLVal) =>
            getType(insideLVal) match {
                case NonErasedPairType(_, t) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
                case t => {
                    SecondErrorType(t)(defaultPos)
                }
            }
        case e: Expr => getType(e: Expr)
    }

    def getType(expr: Expr)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): WACCType = expr match {
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
        case e @ Not(_)    => 
            verifyUnary(e)
            BoolType()(defaultPos)
        case e @ (Negate(_) | Len(_) | Ord(_)) => 
            verifyUnary(e)
            IntType()(defaultPos)
        case e @ Chr(_)    => 
            verifyUnary(e)
            CharType()(defaultPos)

        // Binary operators producing Int results
        case e @(Mul(_, _) | Div(_, _) | Mod(_, _) | Add(_, _) | Sub(_, _)) =>
            verifyBinary(e)
            IntType()(defaultPos)

        // Binary operators producing Bool results
        case e @ (Less(_, _) | LessEqual(_, _) | Greater(_, _) | GreaterEqual(_, _) |
            Equal(_, _) | NotEqual(_, _) | And(_, _) | Or(_, _)) =>
            verifyBinary(e)
            BoolType()(defaultPos)

        // Identifiers
        case (id @ Ident(name)) =>
            st.lookupSymbol(name) match {
                case Some(t) => t
                case None => {
                    errors +=
                        genSpecializedError(
                          Seq(
                            s"Scope error: variable $name has not been declared in this scope"
                          ),
                          id.pos
                        )
                    anyType
                }
            }

        // Array elements
        case ArrayElem(id, _) =>
            getType(id: Expr) match {
                case ArrayType(t) => t
                case t => {
                    ArrayErrorType(t)(defaultPos)
                }
            }
    }
    private def verifyTypeHelper(t: WACCType, expT: Seq[WACCType], pos: (Int, Int))(implicit
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit =
        if (expT.forall(!compatible(t, _)))
            errors +=
                genVanillaError(
                  s"${t.toString()}",
                  expT.mkString(", "),
                  Seq(),
                  pos
                )

    private def verifyType(e: LValue, expT: WACCType*)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT, e.pos)

    private def verifyType(e: RValue, expT: WACCType*)(implicit
        st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT, e.pos)

    private def verifyType(e: Expr, expT: WACCType*)(implicit
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
        case Negate(e) => verifyType(e, BoolType()(defaultPos))
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
        case Read(e)    => verifyType(e, anyType)
        case Declare(t, Ident(name), v) => {
            if (!st.addSymbol(name, t)) {
                errors +=
                    genSpecializedError(
                      Seq(
                        s"Scope error: illegal redeclaration of variable $name "
                      ),
                      stmt.pos
                    )
            }
            verifyType(v, t)
        }
        case Assign(v1, v2) =>
            (getType(v1), getType(v2)) match {
                case (UnknownType(), UnknownType()) =>
                    errors +=
                        genSpecializedError(
                          Seq(
                            "Type error: attempting to exchange values between pairs of unknown types",
                            "pair exchange is only legal when the type of at least one of the sides is known or specified"
                          ),
                          stmt.pos
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
                    genSpecializedError(
                        Seq(
                        "Return Placement Error: return outside of function is not allowed"
                        ),
                        stmt.pos
                    )
    }
}
