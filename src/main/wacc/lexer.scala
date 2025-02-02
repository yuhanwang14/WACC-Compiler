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
                "newpair",
                "fst", 
                "snd",
                "null"
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
            ),
            graphicCharacter
                = Unicode(x => x >= ' '.toInt & x != '\"'.toInt & x != '\''.toInt & x != '\\'.toInt)
        )
    )
    private val lexer = Lexer(desc)

    lazy val intLiter = lexer.lexeme.integer.decimal
    lazy val charLiter = lexer.lexeme.character.ascii
    lazy val strLiter = lexer.lexeme.string.ascii
    lazy val ident = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
