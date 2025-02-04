package errors

sealed trait LineError {
    val errorType: String
    val msgs: Seq[String]
    val lines: Seq[String]

    protected def mergeExpectation(
        unexpected: Option[String],
        expected: Option[String],
        reasons: Seq[String]
    ): Seq[String] = {
        (unexpected, expected) match {
            case (None, None) => reasons
            case _ =>
                ("unexpected " + unexpected.getOrElse("")) +:
                    ("expected " + expected.getOrElse("")) +:
                    reasons
        }
    }
}

case class SyntaxError(
    unexpected: Option[String],
    expected: Option[String],
    reasons: Seq[String],
    lineInfo: LineInfo
) extends LineError {
    override val errorType: String = "syntax error"
    override val msgs: Seq[String] =
        mergeExpectation(unexpected, expected, reasons)
    override val lines: Seq[String] = lineInfo.format
}

case class SemanticError(
    unexpected: Option[String],
    expected: Option[String],
    reasons: Seq[String],
    lineinfo: LineInfo
) extends LineError {
    override val errorType: String = "semantic error"
    override val msgs: Seq[String] =
        mergeExpectation(unexpected, expected, reasons)
    override val lines: Seq[String] = lineinfo.format
}

