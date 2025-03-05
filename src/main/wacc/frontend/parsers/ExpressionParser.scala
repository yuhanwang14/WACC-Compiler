package frontend.parsers

import frontend.ast.*
import frontend.lexer.* 
import frontend.lexer.implicits.implicitSymbol
import parsley.Parsley.*, parsley.Parsley
import parsley.errors.combinator.*
import parsley.errors.patterns.*
import parsley.expr.{precedence, SOps, Atoms, InfixL, InfixR, InfixN, Prefix}
import parsley.character.{digit}

object ExpressionParser {
  val ident = Ident(identName)
  val arrayElem = atomic(
    ArrayElem(ident, some("[".hide ~> expr <~ "]".hide))
  )
  val atom =
    Atoms(
      arrayElem.label("atom"),
      IntLiter(intLiter).label("atom"),
      BoolLiter("true" #> true | "false" #> false).label("atom"),
      CharLiter(charLiter).label("atom"),
      StrLiter(strLiter).label("atom"),
      PairLiter from "null".label("atom"),
      ident <~ "("
        .preventativeExplain(
          reason = "function calls may not appear in expressions and must use `call`",
          labels = "array index or binary operator"
        )
        .label("atom"),
      Paren("(" ~> expr <~ ")").label("atom")
    )
  val expr: Parsley[Expr] = precedence {
    SOps(InfixR)(Or from "||".label("binary operator")) +:
      SOps(InfixR)(And from "&&".label("binary operator")) +:
      SOps(InfixN)(
        Equal from "==".label("binary operator"),
        NotEqual from "!=".label("binary operator")
      ) +:
      SOps(InfixN)(
        Less from "<".label("binary operator"),
        LessEqual from "<=".label("binary operator"),
        Greater from ">".label("binary operator"),
        GreaterEqual from ">=".label("binary operator")
      ) +:
      SOps(InfixL)(
        Add from "+".label("binary operator"),
        Sub from "-".label("binary operator")
      ) +:
      SOps(InfixL)(
        Mul from "*".label("binary operator"),
        Div from "/".label("binary operator"),
        Mod from "%".label("binary operator")
      ) +:
      SOps(Prefix)(
        Not from "!".label("unary operator"),
        atomic(
          Negate from "-".label("unary operator") <~ notFollowedBy(digit)
        ),
        Len from "len".label("unary operator"),
        Ord from "ord".label("unary operator"),
        Chr from "chr".label("unary operator")
      ) +:
      atom
  }.label("expression")
}
