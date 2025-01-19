package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain

import lexer.implicits.implicitSymbol
import lexer.{integer, fully}

object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    private val parser = fully(expr)
    
    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y

    private lazy val expr: Parsley[BigInt] =
        chain.left1(integer | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )
}
