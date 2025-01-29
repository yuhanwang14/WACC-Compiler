package parsers

import AST.statements.*
import parsley.Parsley.*
import parsers.types_parser.waccType
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.combinator.*
import AST.expressions.*
import parsers.expressions_parser.*
import parsley.Parsley
import parsley.expr.*

object statements_parser {
    lazy val program = Program("begin" ~> many(func), stmt <~ "end")

    lazy val func = atomic(Func(waccType, Ident(ident), "(" ~> option(paramList) <~ ")", "is" ~> stmt <~ "end"))

    lazy val paramList: Parsley[ParamList] = ParamList(sepBy1(param, ","))
    lazy val param = Param(waccType, Ident(ident))

    val skipStmt = "skip" as Skip
    lazy val declareStmt = Declare(waccType, Ident(ident), "=" ~> rValue)
    lazy val assignStmt = Assign(lValue, "=" ~> rValue)
    lazy val readStmt = Read("read" ~> lValue)
    val freeStmt = Free("free" ~> expr)
    val returnStmt = Return("return" ~> expr)
    val exitStmt = Exit("exit" ~> expr)
    val printStmt = Print("print" ~> expr)
    val printlnStmt = Println("println" ~> expr)
    lazy val ifStmt = If("if" ~> expr, "then" ~> stmt, "else" ~> stmt <~ "fi")
    lazy val whileStmt = While("while" ~> expr, "do" ~> stmt <~ "done")
    lazy val beginStmt = Begin("begin" ~> stmt <~ "end")

    val simpleStmt: Parsley[Stmt] = skipStmt | atomic(printlnStmt) | printStmt
    | declareStmt | assignStmt | readStmt | freeStmt 
    | returnStmt | exitStmt | ifStmt | whileStmt | beginStmt
    lazy val delimiterStmt = chain.right1(simpleStmt)(Delimiter from ";")
    lazy val stmt: Parsley[Stmt] = delimiterStmt

    lazy val lValue: Parsley[LValue] = arrayElem | Ident(ident) | pairElem
    lazy val pairElem = PairElem(("fst" ~> lValue) | ("snd" ~> lValue))
    lazy val rValue = expr | arrayLiter | newPair | pairElem | call
    val arrayLiter = ArrayLiter("[" ~> sepBy(expr, ",") <~ "]")
    val newPair = NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
    val call = Call("call" ~> Ident(ident), "(" ~> option(argList) <~ ")")
    val argList = ArgList(sepBy1(expr, ","))
}
