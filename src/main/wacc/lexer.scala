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
        "len",
        "ord",
        "chr",
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
          '0', 'b', 't', 'n', 'f', 'r', '\"', '\'', '\\'
        )
      ),
      graphicCharacter =
        Unicode(x => x >= ' '.toInt & x != '\"'.toInt & x != '\''.toInt & x != '\\'.toInt)
    )
  )
  private val lexer = Lexer(desc)
  val intLiter = lexer.lexeme.integer.decimal32
  val charLiter = lexer.lexeme.character.ascii
  val strLiter = lexer.lexeme.string.ascii
  val identName = lexer.lexeme.names.identifier
  val implicits = lexer.lexeme.symbol.implicits

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
