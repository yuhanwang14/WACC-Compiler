package ast

import utils.*

// Abstract trait to track the positions
abstract trait Positional {
  val pos: (Int, Int)
}

// Type alias of declarations for functions and variables
type TypeAndIdent = (WaccType, Ident)

// Program
case class Program(fs: List[Func], s: Stmt)(val pos: (Int, Int)) extends Positional

// Function
case class Func(ti: TypeAndIdent, ps: Option[ParamList], s: Stmt)(
    val pos: (Int, Int)
) extends Positional

// Parameter List
case class ParamList(ps: List[Param])(val pos: (Int, Int)) extends Positional

// Parameter
case class Param(t: WaccType, i: Ident)(val pos: (Int, Int)) extends Positional

// Statement
sealed trait Stmt extends Positional
case class Skip()(val pos: (Int, Int)) extends Stmt
case class Declare(ti: TypeAndIdent, r: RValue)(val pos: (Int, Int)) extends Stmt
case class Assign(l: LValue, r: RValue)(val pos: (Int, Int)) extends Stmt

// Generic `Read` and type-specific variations
case class Read(l: LValue)(val pos: (Int, Int)) extends Stmt:
  def asI: Stmt = ReadI(l)(pos)
  def asC: Stmt = ReadC(l)(pos)
case class ReadI(l: LValue)(val pos: (Int, Int)) extends Stmt
case class ReadC(l: LValue)(val pos: (Int, Int)) extends Stmt

// Generic `Free` and type-specific variations
case class Free(e: Expr)(val pos: (Int, Int)) extends Stmt:
  def asP: Stmt = FreeP(e)(pos)
  def asA: Stmt = FreeA(e)(pos)
case class FreeP(e: Expr)(val pos: (Int, Int)) extends Stmt
case class FreeA(e: Expr)(val pos: (Int, Int)) extends Stmt
case class Return(e: Expr)(val pos: (Int, Int)) extends Stmt
case class Exit(e: Expr)(val pos: (Int, Int)) extends Stmt

// Generic `Print` and type-specific variations
case class Print(e: Expr)(val pos: (Int, Int)) extends Stmt:
  def asB: Stmt = PrintB(e)(pos)
  def asI: Stmt = PrintI(e)(pos)
  def asC: Stmt = PrintC(e)(pos)
  def asS: Stmt = PrintS(e)(pos)
  def asP: Stmt = PrintP(e)(pos)
  def asA: Stmt = PrintA(e)(pos)
case class PrintB(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintI(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintC(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintS(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintP(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintA(e: Expr)(val pos: (Int, Int)) extends Stmt

// Generic `Println` and type-specific variations
case class Println(e: Expr)(val pos: (Int, Int)) extends Stmt:
  def asB: Stmt = PrintlnB(e)(pos)
  def asI: Stmt = PrintlnI(e)(pos)
  def asC: Stmt = PrintlnC(e)(pos)
  def asS: Stmt = PrintlnS(e)(pos)
  def asP: Stmt = PrintlnP(e)(pos)
  def asA: Stmt = PrintlnA(e)(pos)
case class PrintlnB(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintlnI(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintlnC(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintlnS(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintlnP(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintlnA(e: Expr)(val pos: (Int, Int)) extends Stmt

case class If(cond: Expr, t: Stmt, e: Stmt)(val pos: (Int, Int)) extends Stmt
case class While(cond: Expr, s: Stmt)(val pos: (Int, Int)) extends Stmt
case class Begin(s: Stmt)(val pos: (Int, Int)) extends Stmt

case class Block(sts: List[Stmt])(val pos: (Int, Int)) extends Stmt

// Left Value
abstract trait LValue extends Positional
// Ident
// ArrayElem
trait PairElem extends LValue with RValue
case class First(v: LValue)(val pos: (Int, Int)) extends PairElem
case class Second(v: LValue)(val pos: (Int, Int)) extends PairElem

// Right Value
abstract trait RValue extends Positional
// Expr
case class ArrayLiter(es: List[Expr])(val pos: (Int, Int)) extends RValue:
  def toB = ArrayLiterB(es)(pos)
  def toC = ArrayLiterC(es)(pos)
  def toI = ArrayLiterI(es)(pos)
  def toS = ArrayLiterS(es)(pos)
  def toP = ArrayLiterP(es)(pos)
  def toA = ArrayLiterA(es)(pos)
case class ArrayLiterB(es: List[Expr])(val pos: (Int, Int)) extends RValue
case class ArrayLiterC(es: List[Expr])(val pos: (Int, Int)) extends RValue
case class ArrayLiterI(es: List[Expr])(val pos: (Int, Int)) extends RValue
case class ArrayLiterS(es: List[Expr])(val pos: (Int, Int)) extends RValue
case class ArrayLiterP(es: List[Expr])(val pos: (Int, Int)) extends RValue
case class ArrayLiterA(es: List[Expr])(val pos: (Int, Int)) extends RValue
case class NewPair(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue:
  def toB = NewPairB(e1, e2)(pos)
  def toC = NewPairC(e1, e2)(pos)
  def toI = NewPairI(e1, e2)(pos)
  def toS = NewPairS(e1, e2)(pos)
  def toP = NewPairP(e1, e2)(pos)
  def toA = NewPairA(e1, e2)(pos)
case class NewPairB(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
case class NewPairC(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
case class NewPairI(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
case class NewPairS(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
case class NewPairP(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
case class NewPairA(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
// PairElem
case class Call(i: Ident, argL: ArgList)(val pos: (Int, Int)) extends RValue

// Arguments List
case class ArgList(es: List[Expr])(val pos: (Int, Int)) extends Positional

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
