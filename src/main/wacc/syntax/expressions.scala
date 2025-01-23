package syntax

import utils.ParserBridgePos1
import utils.ParserBridgePos2
import parsers.statements.*

object expressions {
    abstract trait Expr extends RValue
    // Using subtypes to avoid useless constructors
    sealed trait Expr1 extends Expr
    case class Or(x: Expr2, y: Expr1)(val pos: (Int, Int)) extends Expr1

    sealed trait Expr2 extends Expr1
    case class And(x: Expr3, y: Expr2)(val pos: (Int, Int)) extends Expr2

    sealed trait Expr3 extends Expr2
    case class Equal(x: Expr4, y: Expr4)(val pos: (Int, Int)) extends Expr3
    case class NotEqual(x: Expr4, y: Expr4)(val pos: (Int, Int)) extends Expr3

    sealed trait Expr4 extends Expr3
    case class Less(x: Expr5, y: Expr5)(val pos: (Int, Int)) extends Expr4
    case class LessEqual(x: Expr5, y: Expr5)(val pos: (Int, Int)) extends Expr4
    case class Greater(x: Expr5, y: Expr5)(val pos: (Int, Int)) extends Expr4
    case class GreaterEqual(x: Expr5, y: Expr5)(val pos: (Int, Int)) extends Expr4

    sealed trait Expr5 extends Expr4
    case class Add(x: Expr5, y: Expr6)(val pos: (Int, Int)) extends Expr5
    case class Sub(x: Expr5, y: Expr6)(val pos: (Int, Int)) extends Expr5

    sealed trait Expr6 extends Expr5
    case class Mul(x: Expr6, y: Atom)(val pos: (Int, Int)) extends Expr6
    case class Div(x: Expr6, y: Atom)(val pos: (Int, Int)) extends Expr6
    case class Mod(x: Expr6, y: Atom)(val pos: (Int, Int)) extends Expr6

    sealed trait Atom extends Expr6
    case class Not(x: Atom)(val pos: (Int, Int)) extends Atom
    case class Negate(x: Atom)(val pos: (Int, Int)) extends Atom
    case class Len(x: Atom)(val pos: (Int, Int)) extends Atom
    case class Ord(x: Atom)(val pos: (Int, Int)) extends Atom
    case class Chr(x: Atom)(val pos: (Int, Int)) extends Atom

    case class IntLiter(x: BigInt)(val pos: (Int, Int)) extends Atom
    case class BoolLiter(x: Boolean)(val pos: (Int, Int)) extends Atom
    case class CharLiter(c: Char)(val pos: (Int, Int)) extends Atom
    case class StrLiter(s: String)(val pos: (Int, Int)) extends Atom
    case class PairLiter(x: Null)(val pos: (Int, Int)) extends Atom
    case class Ident(name: String)(val pos: (Int, Int)) extends Atom with LValue
    case class ArrayElem(ident: Ident, exprs: List[Expr])(val pos: (Int, Int)) extends Atom with LValue
    case class Paren(x: Expr)(val pos: (Int, Int)) extends Atom

    object Or extends ParserBridgePos2[Expr2, Expr1, Expr1]

    object And extends ParserBridgePos2[Expr3, Expr2, Expr2]

    object Equal extends ParserBridgePos2[Expr4, Expr4, Expr3]
    object NotEqual extends ParserBridgePos2[Expr4, Expr4, Expr3]

    object Less extends ParserBridgePos2[Expr5, Expr5, Expr4]
    object LessEqual extends ParserBridgePos2[Expr5, Expr5, Expr4]
    object Greater extends ParserBridgePos2[Expr5, Expr5, Expr4]
    object GreaterEqual extends ParserBridgePos2[Expr5, Expr5, Expr4]

    object Add extends ParserBridgePos2[Expr5, Expr6, Expr5]
    object Sub extends ParserBridgePos2[Expr5, Expr6, Expr5]

    object Mul extends ParserBridgePos2[Expr6, Atom, Expr6]
    object Div extends ParserBridgePos2[Expr6, Atom, Expr6]
    object Mod extends ParserBridgePos2[Expr6, Atom, Expr6]

    object Not extends ParserBridgePos1[Atom, Atom]
    object Negate extends ParserBridgePos1[Atom, Atom]
    object Len extends ParserBridgePos1[Atom, Atom]
    object Ord extends ParserBridgePos1[Atom, Atom]
    object Chr extends ParserBridgePos1[Atom, Atom]

    object IntLiter extends ParserBridgePos1[BigInt, IntLiter]
    object BoolLiter extends ParserBridgePos1[Boolean, BoolLiter]
    object CharLiter extends ParserBridgePos1[Char, CharLiter]
    object StrLiter extends ParserBridgePos1[String, StrLiter]
    object PairLiter extends ParserBridgePos1[Null, PairLiter]
    object Ident extends ParserBridgePos1[String, Ident]
    object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]
    object Paren extends ParserBridgePos1[Expr, Paren]
}
