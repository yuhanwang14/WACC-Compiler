package common

import scala.collection.mutable.Map
import ast.WaccType

/** A variable name shadower in a scope. Translates an identifier to its full prefixed name.
  *
  * This class is essentially a container of [[scala.collection.mutable.Map]].
  *
  * Example:
  * {{{
  * val shadower = new Shadower
  * shadower := "scope1::var1"
  * println(shadower("var1"))   // scope1::var1
  * shadower := "scope1::_scope::var1"
  * println(shadower("var1"))   // scope1::_scope::var1
  * }}}
  *
  * Use [[Shadower.ofScope]] to initialize a `Shadower` instance from a [[Scope]].
  */
class Shadower(scope: Scope) {
  private val dict: Map[String, (String, Int)] = Map()
  scope.varTable.map(put)

  /** Same with [[update]]
    *
    * @param prefixedId
    */
  def :=(identifier: String, varType: WaccType): Boolean = put(identifier, varType)

  /** Takes a prefixed identifier to shadows its identifier. The identifier may or may not already
    * exist in the scope.
    *
    * Though this should not happen after initialization, `update` will not attempt to update an
    * existing "deeper" identifier in the scope.
    *
    * This method is equivalent the `:=` operator.
    *
    * @param prefixedId
    * @return
    *   if the shadowing succeeds
    */
  def put(identifier: String, varType: WaccType): Boolean =
    scope.addSymbol(identifier, varType) match {
      case Some(prefixedId) =>
        val dir: Array[String] = prefixedId.split(Scope.PrefixSeparator)
        val id = dir.last
        if (!dict.contains(id) || dict(id)(1) < dir.length)
          dict(id) = (prefixedId, dir.length)
          true
        else false
      case None =>
        false
    }

  /** Returns type associated with the given identifier or `None` if identifier does not exist in
    * scope.
    */
  def apply(identifer: String): Option[WaccType] =
    // The prefixed identifier, if found, is guaranteed to exist in scope, so `get` is applied here
    dict.get(identifer).map(_(0)).map(scope.lookupSymbol).map(_.get)
}
