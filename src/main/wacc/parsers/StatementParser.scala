package parsers

import ast.*
import parsley.Parsley.*
import parsers.TypeParser.waccType
import wacc.lexer.implicits.implicitSymbol, wacc.lexer.*
import parsley.combinator.*
import parsley.lift.*
import parsers.ExpressionParser.*
import parsley.Parsley
import parsley.errors.combinator.*
import parsley.errors.patterns.*

object StatementParser {
    lazy val program = Program("begin" ~> many(func), block <~ "end")

    val typeAndIdent = waccType <~> Ident(ident)

    lazy val func = 
        Func(
            atomic(typeAndIdent <~ "("),
            option(paramList) <~ ")", 
            "is" ~> _returningBlock <~ "end".hide
        ).label("function")

    lazy val paramList: Parsley[ParamList] = ParamList(sepBy1(param, ","))
    lazy val param = Param(waccType, Ident(ident))

    val skipStmt = Skip from "skip"
    lazy val declareStmt = Declare(typeAndIdent <~ "(".preventativeExplain(
        reason = "all functions must be declared at the top of the main block",
        labels = "statement"
    ), "=" ~> rValue)
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

    lazy val _returningIfStmt = If("if" ~> expr, "then" ~> _returningBlock, "else" ~> _returningBlock <~ "fi")
    lazy val _returningBeginStmt = Begin("begin" ~> _returningBlock <~ "end")
    lazy val _returningStmts: Parsley[Stmt] = returnStmt | exitStmt | _returningIfStmt | _returningBeginStmt
    lazy val _returningBlock = Block(lift2[List[Stmt], Stmt, List[Stmt]](
        _ :+ _, 
        many(atomic(simpleStmt <~ ";")), 
        _returningStmts.explain("function is missing a return on all exit paths")
    ))

    val simpleStmt: Parsley[Stmt] = (skipStmt | printlnStmt | printStmt
    | declareStmt | assignStmt | readStmt | freeStmt 
    | returnStmt | exitStmt | ifStmt | whileStmt | beginStmt).label("statement")
    lazy val block = Block(sepBy1(simpleStmt, ";"))

    lazy val lValue: Parsley[LValue] = arrayElem | Ident(ident) | pairElem
    lazy val pairElem: Parsley[PairElem] = First(("fst" ~> lValue.label("pair"))) | Second(("snd" ~> lValue.label("pair")))
    lazy val rValue = expr | arrayLiter | newPair | pairElem | call
    val arrayLiter = ArrayLiter("[" ~> sepBy(expr, ",") <~ "]").label("array literal")
    val newPair = NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
    val call = Call("call" ~> Ident(ident), "(" ~> argList <~ ")")
    val argList = ArgList(sepBy(expr, ","))
}
