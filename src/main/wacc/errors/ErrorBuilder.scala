package errors

object ErrorBuilder {
    def vanillaError(
        unexpected: String,
        expected: String,
        reasons: Seq[String],
        pos: (Int, Int)
    )(implicit
        lines: Seq[String],
        source: String
    ): Error = genericError(Some(unexpected), Some(expected), reasons, pos)

    def specializedError(
        msgs: Seq[String],
        pos: (Int, Int)
    )(implicit
        lines: Seq[String],
        source: String
    ): Error = genericError(None, None, msgs, pos)

    private def genericError(
        unexpected: Option[String],
        expected: Option[String],
        reasons: Seq[String],
        pos: (Int, Int)
    )(implicit
        lines: Seq[String],
        source: String
    ): Error = Error(
      pos,
      Some(source),
      SemanticError(unexpected, expected, reasons, LineInfo(pos, lines))
    )
}
