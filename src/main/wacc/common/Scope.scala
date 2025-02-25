package common

import scala.collection.mutable.{ArrayBuffer, Map}
import ast.WaccType

/** A `Scope` instance stores all variables and their respective types in a scope. This includes
  * variables from higher scopes.
  *
  * Name shadowing is done through scope prefix, e.g. `_global::_scope::_scope::var1`,
  * `_func_isEven::retVal`. There should be a separate map translating `var1` to
  * `_global::_scope::_scope::var1`.
  */
trait Scope {
  val prefix: String
  val children: ArrayBuffer[Scope] = ArrayBuffer()
  val varTable: Map[String, WaccType] = Map()
  val returnType: Option[WaccType] = None
  val parent: Option[Scope] = None

  /** Adds a new [[ChildScope]] to [[children]] with the given `identifier`.
    *
    * Anonymous child scope has the default name.
    *
    * @return
    *   the newly added child scope
    */
  def addChild(identifier: String = Scope.ScopeDefaultName) = {
    val newChild = ChildScope(this, identifier)
    children.addOne(this)
    newChild
  }

  def addSymbol(identifier: String, varType: WaccType): Option[String] = {
    val prefixedId = prefix + Scope.PrefixSeparator + identifier
    if (varTable.contains(prefixedId))
      None
    else {
      varTable(prefixedId) = varType
      Some(prefixedId)
    }
  }

  def lookupSymbol(prefixedId: String): Option[WaccType] = varTable.get(prefixedId)

  def apply(prefixedId: String): Option[WaccType] = lookupSymbol(prefixedId)
}

object Scope {
  val PrefixSeparator = "::"
  val ScopeDefaultName = "_scope"
}

class ChildScope(parentScope: Scope, identifier: String = Scope.ScopeDefaultName) extends Scope {
  override val parent: Option[Scope] = Some(parentScope)
  override val prefix: String = parent.get.prefix + Scope.PrefixSeparator + identifier
  override val varTable: Map[String, WaccType] = parent.get.varTable
  override val returnType: Option[WaccType] = parent.get.returnType
}

class GlobalScope extends Scope {
  override val prefix = "_global"
}

class FunctionScope(identifer: String, override val returnType: Option[WaccType], paramList: Iterable[(String, WaccType)]) extends Scope {
  override val prefix = f"_func_$identifer"
  paramList.map(addSymbol)
}
