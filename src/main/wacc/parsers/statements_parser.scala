package parsers

import AST.statements.*
import parsley.Parsley.*
import parsers.types_parser.waccType
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.combinator.*
import AST.expressions.*
import parsers.expressions_parser.*
import parsley.Parsley
import parsley.errors.combinator.*
import syntax_checkers.returning_checker._returningBlock

object statements_parser {
    lazy val program = Program("begin" ~> many(func), block <~ "end")

    lazy val func = atomic(Func(waccType, Ident(ident), "(" ~> option(paramList) <~ ")", "is" ~> _returningBlock <~ "end".hide))

    lazy val paramList: Parsley[ParamList] = ParamList(sepBy1(param, ","))
    lazy val param = Param(waccType, Ident(ident))

    val skipStmt = Skip from "skip"
    lazy val declareStmt = Declare(waccType, Ident(ident), "=" ~> rValue)
    lazy val assignStmt = Assign(lValue, "=".label("assignment") ~> rValue)
    lazy val readStmt = Read("read" ~> lValue)
    val freeStmt = Free("free" ~> expr)
    val returnStmt = Return("return" ~> expr)
    val exitStmt = Exit("exit" ~> expr)
    val printStmt = Print("print" ~> expr)
    val printlnStmt = Println("println" ~> expr)
    lazy val ifStmt = If("if" ~> expr, "then" ~> block, "else" ~> block <~ "fi")
    lazy val whileStmt = While("while" ~> expr, "do" ~> block <~ "done")
    lazy val beginStmt = Begin("begin" ~> block <~ "end")

    val simpleStmt: Parsley[Stmt] = (skipStmt | printlnStmt | printStmt
    | declareStmt | assignStmt | readStmt | freeStmt 
    | returnStmt | exitStmt | ifStmt | whileStmt | beginStmt).label("statement")
    lazy val block = Block(sepBy1(simpleStmt, ";".hide))

    lazy val lValue: Parsley[LValue] = arrayElem | Ident(ident) | pairElem
    lazy val pairElem: Parsley[PairElem] = First(("fst" ~> lValue.label("pair"))) | Second(("snd" ~> lValue.label("pair")))
    lazy val rValue = expr | arrayLiter | newPair | pairElem | call
    val arrayLiter = ArrayLiter("[" ~> sepBy(expr, ",") <~ "]")
    val newPair = NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
    val call = Call("call" ~> Ident(ident), "(" ~> option(argList) <~ ")")
    val argList = ArgList(sepBy1(expr, ","))
}
