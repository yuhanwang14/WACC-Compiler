package errors

import parsley.errors.* 
import parsley.errors.tokenextractors.TillNextWhitespace

object Errors {

    case class Error(pos: (Int, Int), lines: ErrorLines) {
        
    }

    sealed trait ErrorLines

    case class VanillaError(
        unexpected: Option[String],
        expected: Option[String],
        reasons: Seq[String]) extends ErrorLines

    case class SpecializedError(msgs: Seq[String]) extends ErrorLines

    case class LineInfo(

    )

    class CustomErrorBuilder extends ErrorBuilder[Error] with TillNextWhitespace {

        type ErrorInfoLines = ErrorLines

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
        type LineInfo = Unit

        override def build(
            pos: (Int, Int), 
            source: Option[String], 
            lines: ErrorInfoLines
        ): Error = Error(pos, lines)

        override def vanillaError(
            unexpected: UnexpectedLine, 
            expected: ExpectedLine, 
            reasons: Messages, 
            line: LineInfo
        ): ErrorInfoLines = VanillaError(unexpected, expected, reasons)

        override def specializedError(
            msgs: Messages, 
            line: LineInfo
        ): ErrorInfoLines = SpecializedError(msgs)

        override def lineInfo(
            line: String, 
            linesBefore: Seq[String], 
            linesAfter: Seq[String], 
            lineNum: Int, 
            errorPointsAt: Int, 
            errorWidth: Int
        ): Unit = ()

        override def expected(alts: ExpectedItems): ExpectedLine = alts
        override def unexpected(item: Option[String]): UnexpectedLine = item
        override def combineExpectedItems(alts: Set[String]): ExpectedItems 
            = if alts.isEmpty then None else Some(alts.mkString(","))


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
}