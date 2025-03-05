package ast

import utils.*

// Expressions
abstract trait Expr extends RValue

// Binary Operations
sealed trait BinaryOp extends Expr

case class Or(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp

case class And(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp

case class Equal(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp
case class NotEqual(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp

case class Less(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp
case class LessEqual(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp
case class Greater(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp
case class GreaterEqual(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp

case class Add(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp
case class Sub(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp

case class Mul(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp
case class Div(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp
case class Mod(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOp

// Unary Operations
sealed trait UnaryOp extends Expr

case class Not(x: Expr)(val pos: (Int, Int)) extends UnaryOp
case class Negate(x: Expr)(val pos: (Int, Int)) extends UnaryOp
case class Len(x: Expr)(val pos: (Int, Int)) extends UnaryOp
case class Ord(x: Expr)(val pos: (Int, Int)) extends UnaryOp
case class Chr(x: Expr)(val pos: (Int, Int)) extends UnaryOp

// Atoms
case class IntLiter(x: Int)(val pos: (Int, Int)) extends Expr
case class BoolLiter(x: Boolean)(val pos: (Int, Int)) extends Expr
case class CharLiter(c: Char)(val pos: (Int, Int)) extends Expr
case class StrLiter(s: String)(val pos: (Int, Int)) extends Expr
case class PairLiter()(val pos: (Int, Int)) extends Expr
case class Ident(name: String)(val pos: (Int, Int)) extends Expr with LValue:
    def renamed(name: String) = Ident(name)(pos)
case class ArrayElem(identName: Ident, exprs: List[Expr])(val pos: (Int, Int)) extends Expr with LValue
case class Paren(x: Expr)(val pos: (Int, Int)) extends Expr

object Or extends ParserBridgePos2[Expr, Expr, Expr]

object And extends ParserBridgePos2[Expr, Expr, Expr]

object Equal extends ParserBridgePos2[Expr, Expr, Expr]
object NotEqual extends ParserBridgePos2[Expr, Expr, Expr]

object Less extends ParserBridgePos2[Expr, Expr, Expr]
object LessEqual extends ParserBridgePos2[Expr, Expr, Expr]
object Greater extends ParserBridgePos2[Expr, Expr, Expr]
object GreaterEqual extends ParserBridgePos2[Expr, Expr, Expr]

object Add extends ParserBridgePos2[Expr, Expr, Expr]
object Sub extends ParserBridgePos2[Expr, Expr, Expr]

object Mul extends ParserBridgePos2[Expr, Expr, Expr]
object Div extends ParserBridgePos2[Expr, Expr, Expr]
object Mod extends ParserBridgePos2[Expr, Expr, Expr]

object Not extends ParserBridgePos1[Expr, Expr]
object Negate extends ParserBridgePos1[Expr, Expr]
object Len extends ParserBridgePos1[Expr, Expr]
object Ord extends ParserBridgePos1[Expr, Expr]
object Chr extends ParserBridgePos1[Expr, Expr]

object IntLiter extends ParserBridgePos1[Int, IntLiter]
object BoolLiter extends ParserBridgePos1[Boolean, BoolLiter]
object CharLiter extends ParserBridgePos1[Char, CharLiter]
object StrLiter extends ParserBridgePos1[String, StrLiter]
object Ident extends ParserBridgePos1[String, Ident]
object PairLiter extends ParserBridgePos0[PairLiter]
object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]
object Paren extends ParserBridgePos1[Expr, Paren]
