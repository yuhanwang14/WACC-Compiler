package instructions

sealed class AsmSnippet(code: String) {
  override def toString: String = code
}

class Header(code: String) extends AsmSnippet(code)

case class DataHeader() extends Header(".data")

case class TextHeader() extends Header(".text")

case class BssHeader() extends Header(".bss")

case class SectionHeader(name: String) extends Header(f".section $name")

case class ByteConst(value: Byte) extends Header(f".byte $value")

case class WordHeader(value: Int) extends Header(f".word $value")

case class StringHeader(value: String) extends Header(f".asciz \"$value\"")

case class Label(identifier: String) extends AsmSnippet(f"$identifier:")

