package syntaxCheckers

import parsers.statementParser.*
import parsley.lift.*
import ast.*
import parsers.expressionParser.*
import wacc.lexer.implicits.implicitSymbol
import parsley.Parsley.*, parsley.Parsley

object returning_checker {
    lazy val _returningIfStmt = If("if" ~> expr, "then" ~> block, "else" ~> block <~ "fi")
    lazy val _returningStmts: Parsley[Stmt] = returnStmt | exitStmt | _returningIfStmt
    lazy val _returningBlock = Block(lift2[List[Stmt], Stmt, List[Stmt]](
        _ :+ _, 
        many(atomic(simpleStmt <~ ";")), 
        _returningStmts
    ))
}
