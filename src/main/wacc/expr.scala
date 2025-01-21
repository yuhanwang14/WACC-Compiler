package wacc

import parsley.generic.*

object expressions {
    sealed trait Expr

    sealed trait Expr1 extends Expr
    case class Or(x: Expr2, y: Expr1) extends Expr1

    sealed trait Expr2 extends Expr1
    case class And(x: Expr3, y: Expr2) extends Expr2

    sealed trait Expr3 extends Expr2
    case class Equal(x: Expr4, y: Expr4) extends Expr3
    case class NotEqual(x: Expr4, y: Expr4) extends Expr3

    sealed trait Expr4 extends Expr3
    case class Less(x: Expr5, y: Expr5) extends Expr4
    case class LessEqual(x: Expr5, y: Expr5) extends Expr4
    case class Greater(x: Expr5, y: Expr5) extends Expr4
    case class GreaterEqual(x: Expr5, y: Expr5) extends Expr4

    sealed trait Expr5 extends Expr4
    case class Add(x: Expr5, y: Expr6) extends Expr5
    case class Sub(x: Expr5, y: Expr6) extends Expr5

    sealed trait Expr6 extends Expr5
    case class Mul(x: Expr6, y: Atom) extends Expr6
    case class Div(x: Expr6, y: Atom) extends Expr6
    case class Mod(x: Expr6, y: Atom) extends Expr6

    sealed trait Atom extends Expr6
    case class Not(x: Atom) extends Atom
    case class Negate(x: Atom) extends Atom
    case class Len(x: Atom) extends Atom
    case class Ord(x: Atom) extends Atom
    case class Chr(x: Atom) extends Atom

    case class IntLiter(x: BigInt) extends Atom
    case class BoolLiter(x: Boolean) extends Atom
    case class CharLiter(c: Char) extends Atom
    case class StrLiter(s: String) extends Atom
    case class PairLiter(x: Null) extends Atom
    case class Ident(name: String) extends Atom
    case class ArrayElem(ident: Ident, exprs: List[Expr]) extends Atom
    case class Paren(x: Expr) extends Atom

    object Or extends ParserBridge2[Expr2, Expr1, Expr1]

    object And extends ParserBridge2[Expr3, Expr2, Expr2]

    object Equal extends ParserBridge2[Expr4, Expr4, Expr3]
    object NotEqual extends ParserBridge2[Expr4, Expr4, Expr3]

    object Less extends ParserBridge2[Expr5, Expr5, Expr4]
    object LessEqual extends ParserBridge2[Expr5, Expr5, Expr4]
    object Greater extends ParserBridge2[Expr5, Expr5, Expr4]
    object GreaterEqual extends ParserBridge2[Expr5, Expr5, Expr4]

    object Add extends ParserBridge2[Expr5, Expr6, Expr5]
    object Sub extends ParserBridge2[Expr5, Expr6, Expr5]

    object Mul extends ParserBridge2[Expr6, Atom, Expr6]
    object Div extends ParserBridge2[Expr6, Atom, Expr6]
    object Mod extends ParserBridge2[Expr6, Atom, Expr6]

    object Not extends ParserBridge1[Atom, Atom]
    object Negate extends ParserBridge1[Atom, Atom]
    object Len extends ParserBridge1[Atom, Atom]
    object Ord extends ParserBridge1[Atom, Atom]
    object Chr extends ParserBridge1[Atom, Atom]

    object IntLiter extends ParserBridge1[BigInt, IntLiter]
    object BoolLiter extends ParserBridge1[Boolean, BoolLiter]
    object CharLiter extends ParserBridge1[Char, CharLiter]
    object StrLiter extends ParserBridge1[String, StrLiter]
    object PairLiter extends ParserBridge1[Null, PairLiter]
    object Ident extends ParserBridge1[String, Ident]
    object ArrayElem extends ParserBridge2[Ident, List[Expr], ArrayElem]
    object Paren extends ParserBridge1[Expr, Paren]
}
