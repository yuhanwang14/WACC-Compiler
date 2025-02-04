package errors

import parsley.errors.{ErrorBuilder as ParsleyErrorBuilder, tokenextractors}

class CustomParsleyErrorBuilder
    extends ParsleyErrorBuilder[Error]
    with tokenextractors.TillNextWhitespace {

    type ErrorInfoLines = LineError

    type ExpectedItems = Option[String]
    type UnexpectedLine = Option[String]
    type ExpectedLine = Option[String]
    type Source = Option[String]

    type Position = (Int, Int)
    type Message = String
    type Messages = Seq[Message]
    type Raw = String
    type Item = String
    type Named = String
    type EndOfInput = String
    type LineInfo = errors.LineInfo

    override def build(
        pos: (Int, Int),
        source: Option[String],
        lines: ErrorInfoLines
    ): Error = Error(pos, source, lines)

    override def vanillaError(
        unexpected: UnexpectedLine,
        expected: ExpectedLine,
        reasons: Messages,
        line: LineInfo
    ): ErrorInfoLines = SyntaxError(unexpected, expected, reasons, line)

    override def specializedError(
        msgs: Messages,
        line: LineInfo
    ): ErrorInfoLines = SyntaxError(None, None, msgs, line)

    override def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        lineNum: Int,
        errorPointsAt: Int,
        errorWidth: Int
    ): LineInfo =
        LineInfo(line, linesBefore, linesAfter, lineNum, errorPointsAt)

    override def expected(alts: ExpectedItems): ExpectedLine = alts
    override def unexpected(item: Option[String]): UnexpectedLine = item
    override def combineExpectedItems(alts: Set[String]): ExpectedItems =
        if alts.isEmpty then None else Some(alts.mkString(", "))

    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg
    override def combineMessages(alts: Seq[String]): Messages = alts

    override def pos(line: Int, col: Int): Position = (line, col)

    override def source(sourceName: Option[String]): Source = sourceName
    override def raw(item: String): Raw = item
    override def named(item: String): Named = item
    override val endOfInput: EndOfInput = "end of input"
    override val numLinesAfter: Int = 1
    override val numLinesBefore: Int = 1

    override def trimToParserDemand: Boolean = true

}
