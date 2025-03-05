package frontend.parsers

import ExpressionParser.*
import TypeParser.*
import frontend.ast.*
import frontend.lexer.*
import frontend.lexer.implicits.implicitSymbol
import parsley.Parsley.*, parsley.Parsley
import parsley.combinator.*
import parsley.lift.*
import parsley.errors.combinator.*
import parsley.errors.patterns.*

object StatementParser {
  val program = Program("begin" ~> many(func), block <~ "end")

  val typeAndIdent = waccType <~> ident

  val func =
    Func(
      atomic(typeAndIdent <~ "("),
      option(paramList) <~ ")",
      "is" ~> returningBlock <~ "end".hide
    ).label("function")

  lazy val paramList: Parsley[ParamList] = ParamList(sepBy1(param, ","))
  val param = Param(waccType, ident)

  val skipStmt = Skip from "skip"
  val declareStmt = Declare(
    typeAndIdent <~ "(".preventativeExplain(
      reason = "all functions must be declared at the top of the main block",
      labels = "statement"
    ),
    "=" ~> rValue
  )
  val assignStmt = Assign(lValue, "=".label("assignment") ~> rValue)
  val readStmt = Read("read" ~> lValue)
  val freeStmt = Free("free" ~> expr)
  val returnStmt = Return("return" ~> expr)
  val exitStmt = Exit("exit" ~> expr)
  val printStmt = Print("print" ~> expr)
  val printlnStmt = Println("println" ~> expr)
  val ifStmt = If("if" ~> expr, "then" ~> block, "else" ~> block <~ "fi")
  val whileStmt = While("while" ~> expr, "do" ~> block <~ "done")
  val beginStmt = Begin("begin" ~> block <~ "end")

  // Returning block for the function
  val returningIfStmt = If(
    "if" ~> expr,
    "then" ~> returningBlock,
    "else" ~> returningBlock <~ "fi"
  )
  val returningBeginStmt = Begin("begin" ~> returningBlock <~ "end")
  val returningStmts: Parsley[Stmt] =
    returnStmt | exitStmt | returningIfStmt | returningBeginStmt
  lazy val returningBlock = Block(
    lift2[List[Stmt], Stmt, List[Stmt]](
      _ :+ _,
      many(atomic(simpleStmt <~ ";")),
      returningStmts.explain(
        "function is missing a return on all exit paths"
      )
    )
  )

  val simpleStmt: Parsley[Stmt] = (skipStmt | printlnStmt | printStmt
    | declareStmt | assignStmt | readStmt | freeStmt
    | returnStmt | exitStmt | ifStmt | whileStmt | beginStmt)
    .label("statement")
  val block = Block(sepBy1(simpleStmt, ";"))

  lazy val lValue: Parsley[LValue] = arrayElem | ident | pairElem
  lazy val pairElem: Parsley[PairElem] =
    First(("fst" ~> lValue.label("pair"))) |
      Second(("snd" ~> lValue.label("pair")))
  lazy val rValue = expr | arrayLiter | newPair | pairElem | call
  val arrayLiter =
    ArrayLiter("[" ~> sepBy(expr, ",") <~ "]").label("array literal")
  val newPair = NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
  val call = Call("call" ~> ident, "(" ~> argList <~ ")")
  val argList = ArgList(sepBy(expr, ","))
}
