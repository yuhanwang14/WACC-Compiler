package parsers

import AST.expressions.*
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.Parsley.*, parsley.Parsley
import parsley.errors.combinator.*
import parsley.expr.{precedence, SOps, Atoms, InfixL, InfixR, InfixN, Prefix}

object expressions_parser {
    lazy val arrayElem = atomic(ArrayElem(Ident(ident), some("[".hide ~> expr <~ "]".hide)))
    lazy val atom = 
        Atoms(
            arrayElem,
            IntLiter(intLiter), 
            BoolLiter("true" #> true | "false" #> false),
            CharLiter(charLiter), 
            StrLiter(strLiter),
            PairLiter from "null",
            Ident(ident), 
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
                     Negate from "-".label("unary operator"), 
                     Len from "len".label("unary operator"), 
                     Ord from "ord".label("unary operator"), 
                     Chr from "chr".label("unary operator")) +:
        atom
    }.label("expression")
}
