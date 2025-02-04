package semantic_checkers

import ast.types.*
import ast.statements.*
import ast.expressions.*
import errors.errors.*
import errors.generator.*
import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable.Seq as MutableSeq

object semanticChecker {
    val defaultPos: (Int, Int) = (-1, -1)
    val anyType: WACCType = AnyType()(defaultPos)

    def compatible(t1: WACCType, t2: WACCType): Boolean =
        t1 == t2 || ((t1, t2) match {
            case (UnknownType(), _)                    => true
            case (_, UnknownType())                    => true
            case (ArrayType(CharType()), StringType()) => true
            case (ArrayType(tt1), ArrayType(tt2)) =>
                compatible(tt1, tt2) &&
                tt1 != ArrayType(CharType()(defaultPos))(defaultPos) &&
                tt2 != StringType()(defaultPos)
            case (
                  NonErasedPairType(t1_1, t1_2),
                  NonErasedPairType(t2_1, t2_2)
                ) =>
                compatible(t1_1, t2_1) && compatible(t1_2, t2_2) &&
                t1_1 != ArrayType(
                  CharType()(defaultPos)
                ) && t2_1 != StringType()(defaultPos) &&
                t1_2 != ArrayType(
                  CharType()(defaultPos)
                ) && t2_2 != StringType()(defaultPos)
            case (NonErasedPairType(_, _), ErasedPairType()) => true
            case (ErasedPairType(), NonErasedPairType(_, _)) => true
            case (AnyType(), _)                              => true
            case (_, AnyType())                              => true
            case _                                           => false
        })

    def commonAncestor(es: List[Expr])(implicit
        st: SymbolTable,
        errors: MutableSeq[Error],
        lines: Seq[String],
        source: String
    ): WACCType = es.map(getType).distinct match {
        case Nil => ArrayType(anyType)(defaultPos)
        case ts @ (head :: tail) => {
            var resultType = head
            var invalid = false
            breakable {
                tail.foreach { t =>
                    if (compatible(t, resultType)) {
                        resultType = t;
                    } else if (!compatible(resultType, t)) {
                        invalid = true
                        break()
                    }
                }
            }
            if (invalid) {
                errors :+
                    genSpecializedError(
                      Seq(
                        "Type Error: array literal mismatch",
                        s"literal contains mix of ${ts.mkString(",")}"
                      ),
                      es.head.pos
                    )
                NotExistType()(defaultPos)
            } else {
                resultType
            }
        }
    }

    def getType(rVal: RValue)(implicit
        st: SymbolTable,
        errors: MutableSeq[Error],
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
                    errors :+
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
    }

    def getType(lVal: LValue)(implicit
        st: SymbolTable,
        errors: MutableSeq[Error],
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
        errors: MutableSeq[Error],
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
        case Not(_)    => BoolType()(defaultPos)
        case Negate(_) => IntType()(defaultPos)
        case Len(_)    => IntType()(defaultPos)
        case Ord(_)    => IntType()(defaultPos)
        case Chr(_)    => CharType()(defaultPos)

        // Binary operators producing Int results
        case Mul(_, _) | Div(_, _) | Mod(_, _) | Add(_, _) | Sub(_, _) =>
            IntType()(defaultPos)

        // Binary operators producing Bool results
        case Less(_, _) | LessEqual(_, _) | Greater(_, _) | GreaterEqual(_, _) |
            Equal(_, _) | NotEqual(_, _) | And(_, _) | Or(_, _) =>
            BoolType()(defaultPos)

        // Identifiers
        case (id @ Ident(name)) =>
            st.lookupSymbol(name) match {
                case Some(t) => t
                case _ => {
                    errors :+
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
        errors: MutableSeq[Error],
        lines: Seq[String],
        source: String
    ): Unit =
        if (expT.forall(!compatible(_, t)))
            errors :+
                genVanillaError(
                  s"${t.toString()}",
                  expT.toString(),
                  Seq(),
                  pos
                )

    private def verifyType(e: LValue, expT: WACCType*)(implicit
        st: SymbolTable,
        errors: MutableSeq[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT, e.pos)

    private def verifyType(e: RValue, expT: WACCType*)(implicit
        st: SymbolTable,
        errors: MutableSeq[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT, e.pos)

    private def verifyType(e: Expr, expT: WACCType*)(implicit
        st: SymbolTable,
        errors: MutableSeq[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT, e.pos)

    def verifyUnary(expr: UnaryOp)(implicit
        st: SymbolTable,
        errors: MutableSeq[Error],
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
        errors: MutableSeq[Error],
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
        errors: MutableSeq[Error],
        lines: Seq[String],
        source: String
    ): Unit = stmt match {
        case Exit(e) => verifyType(e, BoolType()(defaultPos))
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
        case Declare(t1, Ident(name), v) => {
            verifyType(v, t1)
            if (!st.addSymbol(name, t1)) {
                errors :+
                    genSpecializedError(
                      Seq(
                        s"Scope error: illegal redeclaration of variable $name "
                      ),
                      stmt.pos
                    )
            }
        }
        case Assign(v1, v2) =>
            (getType(v1), getType(v2)) match {
                case (UnknownType(), UnknownType()) =>
                    errors :+
                        genSpecializedError(
                          Seq(
                            "Type error: attempting to exchange values between pairs of unknown types",
                            "pair exchange is only legal when the type of at least one of the sides is known or specified"
                          ),
                          stmt.pos
                        )
                case (_, _) => verifyType(v2, getType(v1))
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
                errors :+
                    genSpecializedError(
                        Seq(
                        "Return Placement Error: return outside of function is not allowed"
                        ),
                        stmt.pos
                    )
    }
}
