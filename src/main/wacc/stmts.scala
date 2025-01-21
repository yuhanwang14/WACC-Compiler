package wacc

import parsley.Parsley
import parsley.genericbridges.*

object Program extends ParserBridge2[List[Func], Stmt, Program]

object Func extends ParserBridge4[Type, Ident, Option[ParamList], Stmt, Func]

object ParamList extends ParserBridge1[List[Param], ParamList]

object Param extends ParserBridge2[Type, Ident, Param]

enum Stmt:
    case Skip
    case Declare(t: Type, i: Ident, r: Rvalue)
    case Assign(l: Lvalue, r: Rvalue)
    case Read(l: Lvalue)
    case Free(e: Expr)
    case Return(e: Expr)
    case Exit(e: Expr)
    case Print(e: Expr)
    case Println(e: Expr)
    case If(cond: Expr, t: Stmt, e: Stmt)
    case While(cond: Expr, s: Stmt)
    case Begin(s: Stmt)
    case Delimeter(s1: Stmt, s2: Stmt)

object Stmt:
    object Skip extends ParserBridge0[Stmt]
    object Declare extends ParserBridge3[Type, Ident, Rvalue, Stmt]
    object Assign extends ParserBridge2[Lvalue, Rvalue, Stmt]
    object Read extends ParserBridge1[LValue, Stmt]
    object Free extends ParserBridge1[Expr, Stmt]
    object Return extends ParserBridge1[Expr, Stmt]
    object Exit extends ParserBridge1[Expr, Stmt]
    object Print extends ParserBridge1[Expr, Stmt]
    object Println extends ParserBridge1[Expr, Stmt]
    object If extends ParserBridge3[Expr, Stmt, Stmt, Stmt]
    object While extends ParserBridge2[Expr, Stmt, Stmt]
    object Begin extends ParserBridge1[Stmt, Stmt]
    object Deliobject Lvalue:
    object eter extends ParserBridge2[Stmt, Stmt, Stmt]

enum Rvalue:
    case Expr
    case ArrayLitter
    case Newpair(e1: Expr, e2: Expr)
    case PairElem
    case Call(i: Ident, argList: Option[ArgList])
    // TODO

object ArgList extends ParserBridge2[List[Expr], ArgList]

enum PairElem:
    case Fst(l: Lvalue)
    case Snd(l: Lvalue)

object PairElem:
    object Fst extends ParserBridge1[Lvalue]
    object Snd extends ParserBridge1[Lvalue]

object ArrayLiter extends ParserBridge1[Option[ArgList], ArrayLiter]
