package errors

case class LineInfo(
    line: String,
    linesBefore: Seq[String],
    linesAfter: Seq[String],
    lineNum: Int,
    errorPointsAt: Int
) {
  def format: Seq[String] = {

    def addLineNums(lines: Seq[String], offset: Int): Seq[String] = {
      lines.zipWithIndex.map { case (str, i) =>
        s"""${offset + i} | $str"""
      }
    }

    val offset: Int = linesBefore.size
    val formattedLinesBefore: Seq[String] =
      addLineNums(linesBefore, lineNum - offset)
    val formattedLinesAfter: Seq[String] =
      addLineNums(linesAfter, lineNum + 1)
    val formattedLine: Seq[String] = addLineNums(Seq(line), lineNum)

    val lineNumOffset: Int = lineNum.toString().length() + 3
    val tagLine: String = " " * (lineNumOffset + errorPointsAt) + "^"

    formattedLinesBefore ++ (formattedLine :+ tagLine) ++ formattedLinesAfter
  }
}

object LineInfo {
  def apply(
      pos: (Int, Int),
      lines: Seq[String]
  ) = {
    val row: Int = pos._1
    val col: Int = pos._2
    val len: Int = lines.size
    val width: Int = 1

    val rowStartAt: Int = math.max(row - width, 1)
    val rowEndAt: Int = math.min(row + width, len)
    val linesBefore: Seq[String] =
      if (rowStartAt < row) then lines.slice(rowStartAt - 1, row - 1)
      else Seq[String]()
    val linesAfter: Seq[String] =
      if (row < rowEndAt) then lines.slice(row, rowEndAt)
      else Seq[String]()

    new LineInfo(lines(row - 1), linesBefore, linesAfter, row, col - 1)
  }
}
