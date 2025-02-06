package semanticCheckers

import ast.*
import SemanticChecker.anyType
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

//  scopes are introduced by ‘begin .. end’, functions, if-statements, and while-loops

class SymbolTable {
  // Initialize with a global scope
  private val varTable = mutable.Stack[mutable.Map[String, WaccType]]()
  private val funcTable = mutable.Map[String, FunctionSignature]()
  private var returnType: WaccType = anyType
  private var globalScopeFlag: Boolean = true

  def getVarTable() = varTable

  def getFuncTable() = funcTable

  def getReturnType() = returnType

  def isGlobalScope() = globalScopeFlag

  def setGlobalScope(x: Boolean): Unit = globalScopeFlag = x

  def setReturnType(t: WaccType): Unit = { returnType = t }

  def clearReturnType(): Unit = { returnType = anyType }

  // Enters a new nested scope
  def enterScope(): Unit = varTable.push(mutable.Map())

  // Exits the current scope but ensures the global scope is never removed
  def exitScope(): Unit = varTable.pop()

  // Adds a symbol to the current scope
  def addSymbol(
      name: String,
      typ: WaccType
  ): Boolean =
    if (varTable.head.contains(name)) {
      varTable.head(name) = typ
      false
    } else {
      varTable.head(name) = typ
      true
    }

  // Looks up a symbol from innermost to outermost scope
  def lookupSymbol(name: String): Option[WaccType] = {
    var flag = false
    var result: WaccType = anyType
    breakable {
      for (scope <- varTable) {
        if (scope.contains(name)) {
          result = scope(name)
          flag = true
          break()
        }
      }
    }
    if (flag) Some(result) else None
  }

  def addFunction(f: Func): Boolean = {
    val name = f.ti._2.name
    val returnType = f.ti._1
    val paramTypes = f.ps match {
      case Some(paramList) => paramList.ps.map(p => p.t)
      case None            => List()
    }

    if (funcTable.contains(name)) {
      false
    } else {
      funcTable(name) = FunctionSignature(returnType, paramTypes)
      true
    }
  }

  def lookupFunction(name: String): Option[FunctionSignature] =
    funcTable.get(name)
}

case class FunctionSignature(returnType: WaccType, paramTypes: List[WaccType])
