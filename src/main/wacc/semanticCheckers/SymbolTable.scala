package semanticCheckers

import scala.collection.mutable
import ast.*
import ast.Func
import SemanticChecker.anyType
import scala.util.control.Breaks.{break, breakable}
import SemanticChecker.defaultPos

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

    def setReturnType(t: WaccType) : Unit = {returnType = t}

    def clearReturnType() : Unit = {returnType = anyType}

    // Enters a new nested scope
    def enterScope(): Unit = varTable.push(mutable.Map())

    // Exits the current scope but ensures the global scope is never removed
    def exitScope(): Unit = varTable.pop()

    // Adds a symbol to the current scope
    def addSymbol(name: String, typ: WaccType): Boolean = // change output to returning SemanticError
        if (varTable.head.contains(name)) { // Already declared in current scope
            varTable.head(name) = typ
            false
        }
        else {
            varTable.head(name) = typ
            true
        }

    // Looks up a symbol from innermost to outermost scope
    def lookupSymbol(name: String): Option[WaccType] = {
        var flag = false
        var result: WaccType = AnyType()(defaultPos)
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
    
    def addFunction(f: Func): Boolean = { // change output to returning SemanticError
        val name = f.i.name
        val returnType = f.t
        val paramTypes = f.ps match {
            case Some(paramList) => paramList.ps.map(p => p.t)
            case None => List()
        }

        if (funcTable.contains(name)) { // Function already exists
            false
        } 
        else {
            funcTable(name) = FunctionSignature(returnType, paramTypes)
            true
        }
    }

    def lookupFunction(name: String): Option[FunctionSignature] = funcTable.get(name)
}

case class FunctionSignature(returnType: WaccType, paramTypes: List[WaccType])
