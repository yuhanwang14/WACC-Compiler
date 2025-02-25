package common

import ast.*
import scala.collection.mutable.{ArrayBuffer, Map}

/** A `SymbolTable` instance stores all already-added variables and function signature.
  *
  * The instance also stores the current scope.
  */
class SymbolTable {
  case class FunctionSignature(returnType: WaccType, paramTypes: Seq[WaccType])

  val funcTable: Map[String, (FunctionSignature, FunctionScope)] = Map()
  val funcScopes: ArrayBuffer[FunctionScope] = ArrayBuffer()
  val globalScope: Scope = GlobalScope()
  var currentScope: Scope = globalScope
  var shadower: Shadower = Shadower(currentScope)

  def returnType = currentScope.returnType

  def isGlobalScope() = currentScope == globalScope

  /** Enter a new nested scope. */
  def enterScope(): Unit = {
    currentScope = currentScope.addChild()
    shadower = Shadower(currentScope)
  }

  /** Exit the current scope.
    *
    * @throws IllegalStateException
    *   if the current scope is the global scope or a function scope.
    */
  def exitScope(): Unit = {
    currentScope = currentScope.parent.getOrElse(
      throw IllegalStateException("Attempting to exit from a global / function scope.")
    )
    shadower = Shadower(currentScope)
  }

  // Adds a symbol to the current scope
  def addSymbol(
      identifier: String,
      varType: WaccType
  ): Boolean =
    shadower := (identifier, varType)

  // Looks up a symbol from innermost to outermost scope
  def lookupSymbol(identifier: String): Option[WaccType] = shadower(identifier)

  def addFunction(f: Func): Boolean =
    if (funcTable.contains(f.ti(1).name)) { false }
    else {
      // Deconstruct a `Func` instance
      val identifier: String = f.ti(1).name
      val returnType: WaccType = f.ti(0)
      val paramList: List[(String, WaccType)] =
        f.ps.map(_.ps.map(p => (p.i.name, p.t))).getOrElse(List())
      val fScope = FunctionScope(
        identifier,
        Some(returnType),
        paramList
      )
      val fSignature = FunctionSignature(
        returnType,
        paramList.map(_(1))
      )
      funcScopes.addOne(fScope)
      funcTable.addOne(identifier, (fSignature, fScope))
      true
    }

  def lookupFunction(identifer: String): Option[FunctionSignature] =
    funcTable.get(identifer).map(_(0))

  /** Enter a function scope with an identifier.
    *
    * @throws NoSuchElementException
    *   if the identifier is not present in `funcTable`
    */
  def enterFunctionScope(identifier: String): Unit = {
    currentScope = funcTable.getOrElse(identifier, throw IllegalArgumentException())(1)
    shadower = Shadower(currentScope)
  }

}
