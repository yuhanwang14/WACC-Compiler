package instructions

sealed trait Label

case class LocalLabel(identifier: String) extends Label {
  override def toString: String = s".L$identifier"
}

case class GlobalLabel(identifier: String) extends Label {
  override def toString: String = s"$identifier"
}
