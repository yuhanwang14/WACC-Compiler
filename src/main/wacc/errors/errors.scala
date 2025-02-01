package errors

import parsley.errors.* 
import parsley.errors.tokenextractors.TillNextWhitespace

object errors {

    case class Error(pos: (Int, Int), source: Option[String], lines: ErrorLines) {

        def format: String = {
            val errorDesc 
                = source.fold(lines.errorType)(file => s"${lines.errorType} in $file")
            val errorLoc = s"(row ${pos._1}, col ${pos._2})"
            val errorMsgs = lines.msgs.mkString("\n")
            val errorLines = lines.lines.mkString("\n")
            
            s"""$errorDesc
            |$errorLoc
            |$errorMsgs
            |$errorLines""".stripMargin
        }
    
    }

    sealed trait ErrorLines {
        val errorType: String
        val msgs: Seq[String]
        val lines: Seq[String]
    }

    case class SyntexError(
        unexpected: Option[String],
        expected: Option[String],
        reasons: Seq[String],
        lineInfo: LineInfo
    ) extends ErrorLines {
        override val errorType: String = "syntex error"
        override val msgs: Seq[String] = (unexpected, expected) match {
            case (None, None) => reasons
            case _ => 
                ("unexpected: " + unexpected.getOrElse("")) +:
                ("expected: " + expected.getOrElse("")) +:
                reasons
        }
        override val lines: Seq[String] = lineInfo.format
    }

    case class LineInfo(
        line: String, 
        linesBefore: Seq[String], 
        linesAfter: Seq[String], 
        lineNum: Int, 
        errorPointsAt: Int, 
    ) {
        def format: Seq[String] = {

            def addLineNums(lines: Seq[String], offset: Int): Seq[String] = {
                lines.zipWithIndex.map { case (str, i) =>
                    s"""${offset + i} | $str"""
                }
            }

            val offset: Int = linesBefore.size
            val formattedLinesBefore: Seq[String] = addLineNums(linesBefore, lineNum - offset)
            val formattedLinesAfter: Seq[String] = addLineNums(linesAfter, lineNum + 1)
            val formattedLine: Seq[String] = addLineNums(Seq(line), lineNum)

            val lineNumOffset: Int = lineNum.toString().length() + 3
            val tagLine: String = " " * (lineNumOffset + errorPointsAt) + "^"

            formattedLinesBefore ++ (formattedLine :+ tagLine) ++ formattedLinesAfter
        } 
    }

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
        ): ErrorInfoLines = SyntexError(unexpected, expected, reasons, line)

        override def specializedError(
            msgs: Messages, 
            line: LineInfo
        ): ErrorInfoLines = SyntexError(None, None, msgs, line)

        override def lineInfo(
            line: String, 
            linesBefore: Seq[String], 
            linesAfter: Seq[String], 
            lineNum: Int, 
            errorPointsAt: Int, 
            errorWidth: Int
        ): LineInfo = LineInfo(line, linesBefore, linesAfter, lineNum, errorPointsAt)

        override def expected(alts: ExpectedItems): ExpectedLine = alts
        override def unexpected(item: Option[String]): UnexpectedLine = item
        override def combineExpectedItems(alts: Set[String]): ExpectedItems 
            = if alts.isEmpty then None else Some(alts.mkString(", "))


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