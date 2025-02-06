package ast

import utils.*

// Abstract trait to track the positions
abstract trait Position {
  val pos: (Int, Int)
}

// Type alias of declarations for functions and variables
type TypeAndIdent = (WaccType, Ident)

// Program
case class Program(fs: List[Func], s: Stmt)(val pos: (Int, Int)) extends Position

// Function
case class Func(ti: TypeAndIdent, ps: Option[ParamList], s: Stmt)(
    val pos: (Int, Int)
) extends Position

// Parameter List
case class ParamList(ps: List[Param])(val pos: (Int, Int)) extends Position

// Parameter
case class Param(t: WaccType, i: Ident)(val pos: (Int, Int)) extends Position

// Statement
sealed trait Stmt extends Position
case class Skip()(val pos: (Int, Int)) extends Stmt
case class Declare(ti: TypeAndIdent, r: RValue)(val pos: (Int, Int)) extends Stmt
case class Assign(l: LValue, r: RValue)(val pos: (Int, Int)) extends Stmt
case class Read(l: LValue)(val pos: (Int, Int)) extends Stmt
case class Free(e: Expr)(val pos: (Int, Int)) extends Stmt
case class Return(e: Expr)(val pos: (Int, Int)) extends Stmt
case class Exit(e: Expr)(val pos: (Int, Int)) extends Stmt
case class Print(e: Expr)(val pos: (Int, Int)) extends Stmt
case class Println(e: Expr)(val pos: (Int, Int)) extends Stmt
case class If(cond: Expr, t: Stmt, e: Stmt)(val pos: (Int, Int)) extends Stmt
case class While(cond: Expr, s: Stmt)(val pos: (Int, Int)) extends Stmt
case class Begin(s: Stmt)(val pos: (Int, Int)) extends Stmt

case class Block(sts: List[Stmt])(val pos: (Int, Int)) extends Stmt

// Left Value
abstract trait LValue extends Position
// Ident
// ArrayElem
trait PairElem extends LValue with RValue
case class First(v: LValue)(val pos: (Int, Int)) extends PairElem
case class Second(v: LValue)(val pos: (Int, Int)) extends PairElem

// Right Value
abstract trait RValue extends Position
// Expr
case class ArrayLiter(es: List[Expr])(val pos: (Int, Int)) extends RValue
case class NewPair(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
// PairElem
case class Call(i: Ident, argL: ArgList)(val pos: (Int, Int)) extends RValue

// Arguments List
case class ArgList(es: List[Expr])(val pos: (Int, Int)) extends Position

object Program extends ParserBridgePos2[List[Func], Stmt, Program]

object Func extends ParserBridgePos3[TypeAndIdent, Option[ParamList], Stmt, Func]

object ParamList extends ParserBridgePos1[List[Param], ParamList]

object Param extends ParserBridgePos2[WaccType, Ident, Param]

object Skip extends ParserBridgePos0[Stmt]
object Declare extends ParserBridgePos2[TypeAndIdent, RValue, Stmt]
object Assign extends ParserBridgePos2[LValue, RValue, Stmt]
object Read extends ParserBridgePos1[LValue, Stmt]
object Free extends ParserBridgePos1[Expr, Stmt]
object Return extends ParserBridgePos1[Expr, Stmt]
object Exit extends ParserBridgePos1[Expr, Stmt]
object Print extends ParserBridgePos1[Expr, Stmt]
object Println extends ParserBridgePos1[Expr, Stmt]
object If extends ParserBridgePos3[Expr, Stmt, Stmt, Stmt]
object While extends ParserBridgePos2[Expr, Stmt, Stmt]
object Begin extends ParserBridgePos1[Stmt, Stmt]
object Block extends ParserBridgePos1[List[Stmt], Stmt]

object First extends ParserBridgePos1[LValue, PairElem]
object Second extends ParserBridgePos1[LValue, PairElem]

object ArrayLiter extends ParserBridgePos1[List[Expr], RValue]
object NewPair extends ParserBridgePos2[Expr, Expr, RValue]
object Call extends ParserBridgePos2[Ident, ArgList, RValue]

object ArgList extends ParserBridgePos1[List[Expr], ArgList]
