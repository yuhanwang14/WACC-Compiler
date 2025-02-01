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
            "null" #> PairLiter,
            Ident(ident), 
            Paren("(" ~> expr <~ ")")
        )
    lazy val expr: Parsley[Expr] = precedence {
        SOps(InfixR)(Or from "||".label("operator")) +:
        SOps(InfixR)(And from "&&".label("operator")) +:
        SOps(InfixN)(Equal from "==".label("operator"), 
                     NotEqual from "!=".label("operator")) +:
        SOps(InfixN)(Less from "<".label("operator"),  
                     LessEqual from "<=".label("operator"),
                     Greater from ">".label("operator"),  
                     GreaterEqual from ">=".label("operator")) +: 
        SOps(InfixL)(Add from "+".label("operator"),  
                     Sub from "-".label("operator")) +:
        SOps(InfixL)(Mul from "*".label("operator"),  
                     Div from "/".label("operator"),
                     Mod from "%".label("operator")) +:
        SOps(Prefix)(Not from "!", 
                     Negate from "-", 
                     Len from "len", 
                     Ord from "ord", 
                     Chr from "chr") +:
        atom
    }.label("expression")
}
