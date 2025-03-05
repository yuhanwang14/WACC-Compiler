package common

import scala.collection.mutable.{ArrayBuffer, Map}
import ast.{WaccType as FrontEndType, TypeBridge}
import types.WaccType

/** A `Scope` instance stores all variables and their respective types in a scope. This includes
  * variables from higher scopes.
  *
  * Name shadowing is done through scope prefix, e.g. `_global::_scope::_scope::var1`,
  * `_func_isEven::retVal`. There should be a separate map translating `var1` to
  * `_global::_scope::_scope::var1`.
  */
trait Scope:
  val prefix: String
  val children: ArrayBuffer[Scope] = ArrayBuffer()
  val varTable: Map[String, FrontEndType] = Map()
  val returnType: Option[FrontEndType] = None
  protected val parent: Option[Scope] = None
  var shadower: Shadower = Shadower()
  
  /**
    * For register allocator's use.
    */
  val localVars: ArrayBuffer[(String, WaccType)] = ArrayBuffer()

  def localStackSize: Int = localVars.map(_._2.byteSize).sum()

  /** Adds a new [[ChildScope]] to [[children]] with the given `identifier`.
    *
    * Anonymous child scope has the default name.
    *
    * @return
    *   the newly added child scope
    */
  def addChild(identifier: String = Scope.ScopeDefaultName) =
    val newChild = ChildScope(this, identifier)
    children.addOne(newChild)
    newChild

  def addSymbol(identifier: String, varType: FrontEndType): Boolean =
    val prefixedId = prefix + Scope.PrefixSeparator + identifier
    shadower(identifier) = prefixedId
    if (varTable.contains(prefixedId))
      varTable(prefixedId) = varType
      false
    else
      localVars.addOne(prefixedId, TypeBridge.fromAst(varType))
      varTable(prefixedId) = varType
      true

  def lookupSymbol(identifier: String): Option[FrontEndType] =
    shadower(identifier).flatMap(varTable.get)

  def apply(prefixedId: String): Option[FrontEndType] = lookupSymbol(prefixedId)

object Scope:
  val PrefixSeparator = "::"
  val ScopeDefaultName = "_scope"
  val GlobalScopePrefix = "_global"
  val FunctionIdentifierPrefix = "_func_"
  val FunctionParameterPrefix = "_params"

class ChildScope(parentScope: Scope, identifier: String = Scope.ScopeDefaultName) extends Scope:
  override val parent: Option[Scope] = Some(parentScope)
  override val prefix: String = parentScope.prefix + Scope.PrefixSeparator + identifier
  override val varTable: Map[String, FrontEndType] = parentScope.varTable.clone()
  override val returnType: Option[FrontEndType] = parentScope.returnType
  shadower = parentScope.shadower.clone()

class GlobalScope extends Scope:
  override val prefix = Scope.GlobalScopePrefix

class FunctionScope(
    identifer: String,
    override val returnType: Option[FrontEndType],
    paramList: Iterable[(String, FrontEndType)]
) extends Scope:
  override val prefix = Scope.FunctionIdentifierPrefix + identifer
  paramList.map((identifier, varType) =>
    val prefixedId = prefix + Scope.FunctionParameterPrefix + Scope.PrefixSeparator + identifier
    shadower(identifier) = prefixedId
    varTable(prefixedId) = varType
  )
