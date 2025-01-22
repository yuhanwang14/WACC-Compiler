package wacc

import parsley.Parsley
import parsley.generic.*

// examples
sealed trait Expr
case class IntLiteral(value: Int) extends Expr
case class BoolLiteral(value: Boolean) extends Expr
case class Variable(name: String) extends Expr

type Ident = String
type Type = String
type Lvalue = String
type Rvalue = String

case class Param(t: Type, i: Ident)
object Param extends ParserBridge2[Type, Ident, Param]

case class ParamList(ps: List[Param])
object ParamList extends ParserBridge1[List[Param], ParamList]

case class Func(t: Type, i: Ident, ps: Option[ParamList], s: Stmt)
object Func extends ParserBridge4[Type, Ident, Option[ParamList], Stmt, Func]

case class Program(fs: List[Func], s: Stmt)
object Program extends ParserBridge2[List[Func], Stmt, Program]

sealed trait Stmt
case object Skip extends Stmt
case class Declare(t: Type, i: Ident, r: Rvalue) extends Stmt
case class Assign(l: Lvalue, r: Rvalue) extends Stmt
case class Read(l: Lvalue) extends Stmt
case class Free(e: Expr) extends Stmt
case class Return(e: Expr) extends Stmt
case class Exit(e: Expr) extends Stmt
case class Print(e: Expr) extends Stmt
case class Println(e: Expr) extends Stmt
case class If(cond: Expr, t: Stmt, e: Stmt) extends Stmt
case class While(cond: Expr, s: Stmt) extends Stmt
case class Begin(s: Stmt) extends Stmt
case class Delimeter(s1: Stmt, s2: Stmt) extends Stmt

object Stmt:
    object Skip extends ParserBridge0[Stmt]
    object Declare extends ParserBridge3[Type, Ident, Rvalue, Stmt]
    object Assign extends ParserBridge2[Lvalue, Rvalue, Stmt]
    object Read extends ParserBridge1[Lvalue, Stmt]
    object Free extends ParserBridge1[Expr, Stmt]
    object Return extends ParserBridge1[Expr, Stmt]
    object Exit extends ParserBridge1[Expr, Stmt]
    object Print extends ParserBridge1[Expr, Stmt]
    object Println extends ParserBridge1[Expr, Stmt]
    object If extends ParserBridge3[Expr, Stmt, Stmt, Stmt]
    object While extends ParserBridge2[Expr, Stmt, Stmt]
    object Begin extends ParserBridge1[Stmt, Stmt]
    object Delimeter extends ParserBridge2[Stmt, Stmt, Stmt]

sealed trait PairElem
case class Fst(l: Lvalue) extends PairElem
case class Snd(l: Lvalue) extends PairElem

// object PairElem:
//     object Fst extends ParserBridge1[Lvalue, PairElem]
//     object Snd extends ParserBridge1[Lvalue, PairElem]

case class ArgList(es: List[Expr])
object ArgList extends ParserBridge1[List[Expr], ArgList]

case class ArrayLitter(as: Option[ArgList])
object ArrayLitter extends ParserBridge1[Option[ArgList], ArrayLitter]


object stmtsParser {
    import lexer.implicits.implicitSymbol

    private val typ = ""
    private val ident = ""
    private val rvalue: Rvalue = ???
    private val lvalue = ""
    private val expr = ???

    private lazy val skip = Skip
    private lazy val declare = Declare(typ, ident, "=" ~> rvalue)
    private lazy val assign = Assign(lvalue, "=" ~> rvalue)
    private lazy val read = Read("read" ~> lvalue)
    private lazy val free = Free("free" ~> expr)
    private lazy val ret = Return("return" ~> expr)
    private lazy val exit = Exit("exit" ~> expr)
    private lazy val prt = Print("print" ~> expr)
    private lazy val prtln = Println("println" ~> expr)
    private lazy val ifElse = If("if" ~> expr, "then" ~> stmt, "else" ~> stmt <~ "fi")
    private lazy val whileDo = While("while" ~> expr, "do" ~> stmt <~ "done")
    private lazy val begin = Begin("begin" ~> stmt <~ "end")
    private lazy val deli = Delimeter(stmt, ";" ~> stmt)
    private lazy val stmt = skip
                          | declare
                          | assign
                          | read
                          | free
                          | ret
                          | exit
                          | prt
                          | prtln
                          | ifElse
                          | whileDo
                          | begin
                          | deli
}

