package backend.instructions

class AsmSnippet(code: String)(implicit indent: Int):
  override def toString: String = " " * indent + code.replace("\n", "\n" + " " * indent)

case object EmptyAsmSnippet extends AsmSnippet("")(0)

case class Comment(comment: String)(implicit indent: Int) extends AsmSnippet(f"// $comment")

class MultiLineAsmSnippet(lines: AsmSnippet*)(implicit indent: Int)
    extends AsmSnippet(lines.mkString("\n"))

case class AsmFunction(lines: AsmSnippet*) extends MultiLineAsmSnippet(lines*)(0)

class Header(code: String)(implicit indent: Int) extends AsmSnippet(code)

case class DataHeader() extends Header(".data")(0)

case class TextHeader() extends Header(".text")(0)

case class GlobalHeader(name: String) extends Header(f".global $name")(0)

case class AlignHeader(value: Int) extends Header(f".align $value")(0)

case class SectionHeader(name: String) extends Header(f".section $name")(0)

case class ByteConst(value: Byte) extends Header(f".byte $value")(4)

case class WordConst(value: Int) extends Header(f".word $value")(4)

case class StringConst(value: String) extends Header(f".asciz \"$value\"")(4)

case class LabelledStringConst(label: String, value: String)
    extends MultiLineAsmSnippet(
      Comment(f"// length of ${label}")(0),
      WordConst(value.length()),
      LabelHeader(label),
      StringConst(value),
      AlignHeader(4)
    )(0)

case class LabelHeader(identifier: String) extends AsmSnippet(f"$identifier:")(0)
