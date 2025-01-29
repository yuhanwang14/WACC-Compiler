package AST

import AST.expressions.*
import AST.types.*
import utils.*

object statements {
    sealed trait Stmts
    // Program
    case class Program(fs: List[Func], s: Stmt)(val pos: (Int, Int)) extends Stmts

    // Function
    case class Func(t: WACCType, i: Ident, ps: Option[ParamList], s: Stmt)(val pos: (Int, Int)) extends Stmts

    // Parameter List
    case class ParamList(ps: List[Param])(val pos: (Int, Int)) extends Stmts

    // Parameter
    case class Param(t: WACCType, i: Ident)(val pos: (Int, Int)) extends Stmts

    // Statement
    sealed trait Stmt extends Stmts
    case object Skip extends Stmt
    case class Declare(t: WACCType, i: Ident, r: RValue)(val pos: (Int, Int)) extends Stmt
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
    case class Delimiter(s1: Stmt, s2: Stmt)(val pos: (Int, Int)) extends Stmt

    // Left Value
    abstract trait LValue extends Stmts
    // Ident
    // ArrayElem
    case class PairElem(v: LValue)(val pos: (Int, Int)) extends LValue with RValue

    // Right Value
    abstract trait RValue extends Stmts
    // Expr
    case class ArrayLiter(es: List[Expr])(val pos: (Int, Int)) extends RValue
    case class NewPair(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
    // PairElem
    case class Call(i: Ident, argL: Option[ArgList])(val pos: (Int, Int)) extends RValue

    // Arguments List
    case class ArgList(es: List[Expr])(val pos: (Int, Int)) extends Stmts

    object Program extends ParserBridgePos2[List[Func], Stmt, Program]

    object Func extends ParserBridgePos4[WACCType, Ident, Option[ParamList], Stmt, Func]

    object ParamList extends ParserBridgePos1[List[Param], ParamList]

    object Param extends ParserBridgePos2[WACCType, Ident, Param]

    object Declare extends ParserBridgePos3[WACCType, Ident, RValue, Stmt]
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
    object Delimiter extends ParserBridgePos2[Stmt, Stmt, Stmt]

    object PairElem extends ParserBridgePos1[LValue, PairElem]
    
    object ArrayLiter extends ParserBridgePos1[List[Expr], RValue]
    object NewPair extends ParserBridgePos2[Expr, Expr, RValue]
    object Call extends ParserBridgePos2[Ident, Option[ArgList], RValue]

    object ArgList extends ParserBridgePos1[List[Expr], ArgList]
}
