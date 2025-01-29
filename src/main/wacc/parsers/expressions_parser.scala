package parsers

import AST.expressions.*
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.Parsley.*, parsley.Parsley
import parsley.expr.{precedence, SOps, Atoms, InfixL, InfixR, InfixN, Prefix}

object expressions_parser {
    lazy val arrayElem = atomic(ArrayElem(Ident(ident), some("[" ~> expr <~ "]")))
    lazy val atom = 
        Atoms(
            arrayElem,
            IntLiter(intLiter), 
            BoolLiter("true".as(true) | "false".as(false)),
            CharLiter(charLiter), 
            StrLiter(strLiter),
            PairLiter("null".as(null)),
            Ident(ident), 
            Paren("(" ~> expr <~ ")")
        )
    lazy val expr: Parsley[Expr] = precedence {
        SOps(InfixR)(Or from "||") +:
        SOps(InfixR)(And from "&&") +:
        SOps(InfixN)(Equal from "==", 
                     NotEqual from "!=") +:
        SOps(InfixN)(Less from "<",  
                     LessEqual from "<=",
                     Greater from ">",  
                     GreaterEqual from ">=") +: 
        SOps(InfixL)(Add from "+",  
                     Sub from "-") +:
        SOps(InfixL)(Mul from "*",  
                     Div from "/",
                     Mod from "%") +:
        SOps(Prefix)(Not from "!", 
                     Negate from "-", 
                     Len from "len", 
                     Ord from "ord", 
                     Chr from "chr") +:
        atom
    }
}
