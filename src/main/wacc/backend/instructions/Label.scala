package instructions

sealed trait Label

case class LocalLabel(identifier: String) extends Label {
  override def toString: String = s".L$identifier"
}

case class GlobalLabel(identifier: String) extends Label {
  override def toString: String = s"$identifier"
}

case class StringLabel(identifier: String, value: String, isLocal: Boolean = true) extends Label {
  val label = if isLocal then LocalLabel(identifier).toString else identifier
  override def toString: String = s"  .word ${value.length}\n$label:\n    .asciz \"$value\""
}
