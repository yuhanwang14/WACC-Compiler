package common

import scala.collection.mutable.Map

class Shadower {
  val dict: Map[String, String] = Map()
  def update(identifier: String, prefixedId: String): Unit = { dict(identifier) = prefixedId }
  def apply(identifier: String): Option[String] = dict.get(identifier)

  override def clone(): Shadower = {
    val cloned = Shadower()
    cloned.dict.addAll(dict)
    cloned
  }
}

