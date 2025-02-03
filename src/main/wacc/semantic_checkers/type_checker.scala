package semantic_checkers

import AST.types.*
import AST.statements.*
import AST.expressions.*
import errors.errors.*
import errors.generator.*
import scala.util.control.Breaks.{break, breakable}

object type_checker {
    val defaultPos: (Int, Int) = (-1, -1)
    val anyType: WACCType = AnyType()(defaultPos)

    def invariant(t1: WACCType, t2: WACCType): Boolean =
        t1 != ArrayType(CharType()(t1.pos))(t1.pos)
            && t2 != StringType()(t2.pos)

    def weakened(t1: WACCType, t2: WACCType): Boolean = (t1, t2) match
        case (ArrayType(CharType()), StringType())       => true
        case (NonErasedPairType(_, _), ErasedPairType()) => true
        case (ErasedPairType(), NonErasedPairType(_, _)) => true
        case _                                           => false

    def compatible(t1: WACCType, t2: WACCType): Boolean =
        t1 == t2 || weakened(t1, t2) || t1 == AnyType() || t2 == AnyType()

    def commonAncestor(es: List[Expr])(
        implicit st: SymbolTable,
        errors: Seq[Error],
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
                        "Scope Error: array literal mismatch",
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

    def getType(rVal: RValue)(
        implicit st: SymbolTable,
        errors: Seq[Error],
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
    }

    def getType(lVal: LValue)(
        implicit st: SymbolTable,
        errors: Seq[Error],
        lines: Seq[String],
        source: String
    ): WACCType = lVal match {
        case First(insideLVal) =>
            getType(insideLVal) match {
                case NonErasedPairType(t, _) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
                case _                       => ???
            }

        case Second(insideLVal) =>
            getType(insideLVal) match {
                case NonErasedPairType(_, t) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
                case _                       => ???
            }
        case e: Expr => getType(e: Expr)
    }

    def getType(expr: Expr)(implicit
        st: SymbolTable,
        errors: Seq[Error],
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
                case _            => ???
            }
    }
    private def verifyTypeHelper(t: WACCType, expT: Seq[WACCType])(
        implicit errors: Seq[Error],
        lines: Seq[String],
        source: String
    ): Unit =
        if (expT.forall(!compatible(_, t)))
            errors :+
                genVanillaError(
                  s"${t.toString()}",
                  expT.toString(),
                  Seq(),
                  t.pos
                )

    private def verifyType(e: LValue, expT: WACCType*)(
        implicit st: SymbolTable,
        errors: Seq[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT)

    private def verifyType(e: RValue, expT: WACCType*)(
        implicit st: SymbolTable,
        errors: Seq[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT)

    private def verifyType(e: Expr, expT: WACCType*)(
        implicit st: SymbolTable,
        errors: Seq[Error],
        lines: Seq[String],
        source: String
    ): Unit = verifyTypeHelper(getType(e), expT)

    def verifyUnary(expr: UnaryOp)(
        implicit st: SymbolTable,
        errors: Seq[Error],
        lines: Seq[String],
        source: String
    ): Unit = expr match {
        case Not(e)    => verifyType(e, BoolType()(defaultPos))
        case Negate(e) => verifyType(e, BoolType()(defaultPos))
        case Len(e)    => verifyType(e, ArrayType(anyType)(defaultPos))
        case Ord(e)    => verifyType(e, CharType()(defaultPos))
        case Chr(e)    => verifyType(e, IntType()(defaultPos))
    }

    def verifyBinary(expr: BinaryOp)(
        implicit st: SymbolTable,
        errors: Seq[Error],
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
        errors: Seq[Error],
        lines: Seq[String],
        source: String
    ): Unit = stmt match {
        case Exit(e)           => verifyType(e, BoolType()(defaultPos))
        case If(e, stmt1, stmt2) => {
            st.enterScope()
            verifyType(e, BoolType()(defaultPos))
            verifyStmt(stmt1)
            verifyStmt(stmt2)
            st.exitScope()
        }
        case While(e, stmt0) => {
            st.enterScope()
            verifyType(e, BoolType()(defaultPos))
            verifyStmt(stmt0)
            st.exitScope()
        }
        case Print(e)          => verifyType(e, anyType)
        case Println(e)        => verifyType(e, anyType)
        case Read(e)           => verifyType(e, anyType)
        case Declare(t1, Ident(name), r_val) => {
            verifyType(r_val, t1)
            if(!st.addSymbol(name, t1)) {
                errors :+
                genSpecializedError(
                    Seq(
                        s"Scope error: illegal redeclaration of variable $name "
                    ),
                    stmt.pos
                )
            }
        }
        case Assign(v1, v2) => {
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
                case (UnknownType(), t2) => ???
                case (t1, UnknownType()) => ???
                case _ => 

            }
            verifyType(v2, getType(v1))
        }
        case Begin(stmt0)          => {
            st.enterScope()
            verifyStmt(stmt0)
            st.exitScope()
        }
        case Block(sts)        => sts.foreach(verifyStmt)
        case Skip()            =>
        case Free(e) =>
            verifyType(
              e,
              ArrayType(anyType)(defaultPos),
              NonErasedPairType(anyType, anyType)(defaultPos)
            )
        case Return(e) => {
            ???
        }
    }
}
