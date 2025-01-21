package wacc

sealed trait Expr
// Unary Operator
case class NOT(x: Expr) extends Expr
case class NEG(x: Expr) extends Expr
case class LEN(x: Expr) extends Expr
case class ORD(x: Expr) extends Expr
case class CHR(x: Expr) extends Expr
// Binary Operator 
case class MUL(x: Expr, y: Expr) extends Expr
case class DIV(x: Expr, y: Expr) extends Expr
case class MOD(x: Expr, y: Expr) extends Expr
case class ADD(x: Expr, y: Expr) extends Expr
case class SUB(x: Expr, y: Expr) extends Expr
case class GT(x: Expr, y: Expr) extends Expr
case class GE(x: Expr, y: Expr) extends Expr
case class LT(x: Expr, y: Expr) extends Expr
case class LE(x: Expr, y: Expr) extends Expr
case class EQ(x: Expr, y: Expr) extends Expr
case class NE(x: Expr, y: Expr) extends Expr
case class AND(x: Expr, y: Expr) extends Expr
case class OR(x: Expr, y: Expr) extends Expr

sealed trait Atom extends Expr
case class IntLiter(x: BigInt) extends Atom
case class BoolLiter(x: Boolean) extends Atom
case class CharLiter(c: Char) extends Atom
case class StrLiter(s: String) extends Atom
case class PairLiter() extends Atom
case class Ident(name: String) extends Atom
case class ArrayElem(ident: Ident, exprs: List[Expr]) extends Atom
case class Paren(x: Expr) extends Atom
