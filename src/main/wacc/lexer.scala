package wacc

import parsley.Parsley
import parsley.token.*
import parsley.token.descriptions.*

object lexer {
    private val desc = LexicalDesc.plain.copy(
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#"
        ),
        numericDesc = NumericDesc.plain,
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(c => c == '_' || c.isLetter),
            identifierLetter = Basic(c => c == '_' || c.isLetterOrDigit)
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set[String](
                "null",
                "int", 
                "bool",
                "char",
                "string",
                "pair",
                "begin",
                "end",
                "is",
                "skip",
                "read",
                "free",
                "return",
                "exit",
                "println",
                "if",
                "then",
                "else",
                "fi",
                "while",
                "do",
                "done",
                "call", 
                "fst", 
                "snd"
            ), 
            hardOperators = Set[String](
                "!", 
                "-",
                "len",
                "ord",
                "chr", 
                "*", 
                "/", 
                "%",
                "+",
                ">", 
                ">=", 
                "<", 
                "<=", 
                "==", 
                "!=", 
                "&&", 
                "||"
            )
        ),
        textDesc = TextDesc.plain.copy(
            escapeSequences = EscapeDesc.plain.copy(
                literals = Set[Char](
                    '0', 
                    'b', 
                    't', 
                    'n', 
                    'f', 
                    'r',
                    '\"', 
                    '\'', 
                    '\\'
                )
            )
        )
    )
    private val lexer = Lexer(desc)

    val integer = lexer.lexeme.natural.decimal
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
