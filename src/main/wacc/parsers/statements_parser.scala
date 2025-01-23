package parsers

import parsers.statements.*
import parsley.Parsley.*
import parsers.types_parser.waccType
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.combinator.*
import syntax.expressions.*
import parsers.expressions_parser.*
import parsley.expr.chain
import parsley.position.pos
import parsley.Parsley

object statements_parser {
    val program = Program("begin" ~> many(func), stmt <~ "end")

    val func = Func(waccType, Ident(ident), "(" ~> option(paramList) <~ ")", "is" ~> stmt <~ "end")

    val paramList = ParamList(param, many("," ~> param))
    val param = Param(waccType, Ident(ident))

    val skipStmt = "skip" as Skip
    val declareStmt = Declare(waccType, Ident(ident), "=" ~> rValue)
    val assignStmt = Assign(lValue, "=" ~> rValue)
    val readStmt = Read("read" ~> lValue)
    val freeStmt = Free("free" ~> expr)
    val returnStmt = Return("return" ~> expr)
    val exitStmt = Exit("exit" ~> expr)
    val printStmt = Print("print" ~> expr)
    val printlnStmt = Println( "println" ~> expr)
    lazy val ifStmt = If("if" ~> expr, "then" ~> stmt, "else" ~> stmt <~ "fi")
    lazy val whileStmt = While("while" ~> expr, "do" ~> stmt <~ "done")
    lazy val beginStmt = Begin("begin" ~> stmt <~ "end")
    lazy val delimiterStmt = chain.left1(stmt)(Delimiter from ";")

    val stmt: Parsley[Stmt] = skipStmt | declareStmt | assignStmt  
    | readStmt | freeStmt | returnStmt | exitStmt | printStmt 
    | printlnStmt | ifStmt | whileStmt | beginStmt | delimiterStmt
    val lValue: Parsley[LValue] = Ident(ident) | arrayElem | pairElem
    lazy val pairElem = PairElem(("fst" ~> lValue) | ("snd" ~> lValue))
    val rValue = expr | arrayLiter | newPair | pairElem
    val arrayLiter = ArrayLiter("[" ~> option(sepBy1(expr, ",")) <~ "]")
    val newPair = NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
    val call = Call("call" ~> Ident(ident), "(" ~> option(argList) <~ ")")
    val argList = ArgList(sepBy1(expr, ","))
}
