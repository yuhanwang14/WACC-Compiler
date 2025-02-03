package errors
import errors.* 

object generator {

    def genVanillaError(
        unexpected: String,
        expected: String,
        reasons: Seq[String],
        pos: (Int, Int),
    )(
        implicit lines: Seq[String],
        source: String
    ): Error = {
        genError(Some(unexpected), Some(expected), reasons, pos)
    }

    def genSpecializedError(
        msgs: Seq[String],
        pos:(Int, Int),
    )(
        implicit lines: Seq[String],
        source: String
    ): Error = {
        genError(None, None, msgs, pos)
    }

    private def genError(
        unexpected: Option[String],
        expected: Option[String],
        reasons: Seq[String],
        pos: (Int, Int),
    )(
        implicit lines: Seq[String],
        source: String
    ): Error = {
        val lineInfo: LineInfo = genLineInfo(pos, lines)
        val errorLine: SemanticError = 
            new SemanticError(unexpected, expected, reasons, lineInfo)
        return new Error(pos, Some(source), errorLine)
    }

    private def genLineInfo(
        pos: (Int, Int),
        lines: Seq[String]
    ): LineInfo = {
        
        val row: Int = pos._1
        val col: Int = pos._2
        val len: Int = lines.size
        val width: Int = 1
        
        val rowStartAt: Int = if (row - width >= 1) then row - width else 1;
        val rowEndAt: Int = if (row + width <= len) then row + width else len;
        val linesBefore: Seq[String] = 
            if (rowStartAt < row) then lines.slice(rowStartAt - 1, row - 2) else Seq[String]()
        val linesAfter: Seq[String] = 
            if (row < rowEndAt) then lines.slice(row, rowEndAt - 1) else Seq[String]()

        return new LineInfo(lines(row - 1), linesBefore, linesAfter, row, col)
    }
}