package parsers

import ast.*
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.Parsley.*, parsley.Parsley
import parsley.errors.combinator.*
import parsley.expr.{precedence, SOps, Atoms, InfixL, InfixR, InfixN, Prefix}
import parsley.character.{digit}
import parsley.errors.patterns.*
import parsley.errors.VanillaGen

object ExpressionParser {
    lazy val arrayElem = atomic(ArrayElem(Ident(ident), some("[".hide ~> expr <~ "]".hide)))
    lazy val atom = 
        Atoms(
            arrayElem,
            IntLiter(intLiter), 
            BoolLiter("true" #> true | "false" #> false),
            CharLiter(charLiter), 
            StrLiter(strLiter),
            PairLiter from "null",
            Ident(ident) <~ "(".preventWith(
                err = new VanillaGen[Unit] {
                    override def reason(x: Unit) = Some("function calls may not appear in expressions and must use `call`")
                    override def unexpected(x: Unit) = VanillaGen.NamedItem("opening parenthesis")
                },
                labels = "array index or binary operator"
            ), 
            Paren("(" ~> expr <~ ")")
        )
    lazy val expr: Parsley[Expr] = precedence {
        SOps(InfixR)(Or from "||".label("binary operator")) +:
        SOps(InfixR)(And from "&&".label("binary operator")) +:
        SOps(InfixN)(Equal from "==".label("binary operator"), 
                     NotEqual from "!=".label("binary operator")) +:
        SOps(InfixN)(Less from "<".label("binary operator"),  
                     LessEqual from "<=".label("binary operator"),
                     Greater from ">".label("binary operator"),  
                     GreaterEqual from ">=".label("binary operator")) +: 
        SOps(InfixL)(Add from "+".label("binary operator"),  
                     Sub from "-".label("binary operator")) +:
        SOps(InfixL)(Mul from "*".label("binary operator"),  
                     Div from "/".label("binary operator"),
                     Mod from "%".label("binary operator")) +:
        SOps(Prefix)(Not from "!".label("unary operator"), 
                     atomic(Negate from "-".label("unary operator") <~ notFollowedBy(digit)), 
                     Len from "len".label("unary operator"), 
                     Ord from "ord".label("unary operator"), 
                     Chr from "chr".label("unary operator")) +:
        atom
    }.label("expression")
}
