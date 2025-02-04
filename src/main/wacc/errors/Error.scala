package errors

case class Error(pos: (Int, Int), source: Option[String], line: LineError) {
    def format: String = {
        val errorDesc =
            source.fold(line.errorType)(file => s"${line.errorType} in $file")
        val errorLoc = s"(row ${pos._1}, col ${pos._2}):"
        val errorMsgs = line.msgs.mkString("\n  ")
        val lineError = line.lines.mkString("\n  ")

        s"""$errorDesc
            |$errorLoc
            |  $errorMsgs
            |  $lineError""".stripMargin
    }

}
