package backend.instructions

class AsmSnippet(code: String)(implicit indent: Int):
  override def toString: String = " " * indent + code

case object EmptyAsmSnippet extends AsmSnippet("")(0)

case object AsmBlankLine extends AsmSnippet("\n")(0)

case class Comment(comment: String)(implicit indent: Int) extends AsmSnippet(f"// $comment\n")

class MultiLineAsmSnippet(lines: AsmSnippet*)(implicit indent: Int)
    extends AsmSnippet(lines.mkString(""))

case class AsmFunction(lines: AsmSnippet*) extends MultiLineAsmSnippet(lines*)(0)

class Header(code: String)(implicit indent: Int) extends AsmSnippet(code)

case class DataHeader() extends Header(".data\n")(0)

case class TextHeader() extends Header(".text\n")(0)

case class GlobalHeader(name: String) extends Header(f".global $name\n")(0)

case class AlignHeader(value: Int) extends Header(f".align $value\n")(0)

case class SectionHeader(name: String) extends Header(f".section $name\n")(0)

case class ByteConst(value: Byte) extends Header(f".byte $value\n")(4)

case class WordConst(value: Int) extends Header(f".word $value\n")(4)

case class StringConst(value: String) extends Header(f".asciz \"$value\"\n")(4)

case class LabelledStringConst(label: String, value: String)
    extends MultiLineAsmSnippet(
      Comment(f"length of ${label}")(0),
      WordConst(value.length()),
      LabelHeader(label),
      StringConst(value),
      AlignHeader(4)
    )(0)

case class LabelHeader(identifier: String) extends AsmSnippet(f"$identifier:\n")(0)

extension (builder: StringBuilder)
  def appendAll(codes: AsmSnippet | StringBuilder | String*): StringBuilder =
    codes.foldLeft(builder): (_, code) =>
      builder.append(code)
