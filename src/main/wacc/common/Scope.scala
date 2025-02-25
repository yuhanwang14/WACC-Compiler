package common

import scala.collection.mutable.{ArrayBuffer, Map}
import ast.WaccType
import common.Scope.PrefixSeparator

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
  val shadower: Shadower = Shadower()

  /** Adds a new [[ChildScope]] to [[children]] with the given `identifier`.
    *
    * Anonymous child scope has the default name.
    *
    * @return
    *   the newly added child scope
    */
  def addChild(identifier: String = Scope.ScopeDefaultName) = {
    val newChild = ChildScope(this, identifier)
    children.addOne(newChild)
    newChild
  }

  def addSymbol(identifier: String, varType: WaccType): Boolean = {
    val prefixedId = prefix + Scope.PrefixSeparator + identifier
    shadower(identifier) = prefixedId
    if (varTable.contains(prefixedId))
      varTable(prefixedId) = varType
      false
    else {
      varTable(prefixedId) = varType
      true
    }
  }

  /** Shadow an identifier with local variable. Assumes `identifier` already added into `varTable`.
    */
  def shadow(identifier: String): Unit = {
    shadower(identifier) = prefix + PrefixSeparator + identifier
  }

  def lookupSymbol(identifier: String): Option[WaccType] = shadower(identifier).flatMap(varTable.get)

  def apply(prefixedId: String): Option[WaccType] = lookupSymbol(prefixedId)
}

object Scope {
  val PrefixSeparator = "::"
  val ScopeDefaultName = "_scope"
  val GlobalScopePrefix = "_global"
  val FunctionIdentifierPrefix = "_func_"
  val FunctionParameterPrefix = "_params"
}

class ChildScope(parentScope: Scope, identifier: String = Scope.ScopeDefaultName) extends Scope {
  override val parent: Option[Scope] = Some(parentScope)
  override val prefix: String = parent.get.prefix + Scope.PrefixSeparator + identifier
  override val varTable: Map[String, WaccType] = parent.get.varTable.clone()
  override val returnType: Option[WaccType] = parent.get.returnType
  override val shadower: Shadower = parentScope.shadower.clone()
}

class GlobalScope extends Scope {
  override val prefix = Scope.GlobalScopePrefix
}

class FunctionScope(
    identifer: String,
    override val returnType: Option[WaccType],
    paramList: Iterable[(String, WaccType)]
) extends Scope {
  override val prefix = Scope.FunctionIdentifierPrefix + identifer
  paramList.map((identifier, varType) =>
    varTable(prefix + Scope.FunctionParameterPrefix + Scope.PrefixSeparator + identifier) = varType
  )
}
