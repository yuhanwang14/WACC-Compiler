package semantic_checkers

import scala.collection.mutable
import AST.types.*
import AST.statements.Func
import semantic_checkers.semanticChecker.anyType

//  scopes are introduced by ‘begin .. end’, functions, if-statements, and while-loops

class SymbolTable {
    // Initialize with a global scope
    private val varTable = mutable.Stack[mutable.Map[String, WACCType]](mutable.Map())
    private val funcTable = mutable.Map[String, FunctionSignature]()
    private var returnType: WACCType = anyType

    def getVarTable() = varTable

    def getFuncTable() = funcTable

    def getReturnType() = returnType

    def setReturnType(t: WACCType) : Unit = {returnType = t}

    def clearReturnType() : Unit = {returnType = anyType}
    
    def isGlobalScope(): Boolean = varTable.size == 1

    // Enters a new nested scope
    def enterScope(): Unit = varTable.push(mutable.Map())


    // Exits the current scope but ensures the global scope is never removed
    def exitScope(): Unit = {
        if (varTable.size > 1) varTable.pop()
        else throw new RuntimeException("Cannot exit global scope")
    }

    // Adds a symbol to the current scope
    def addSymbol(name: String, typ: WACCType): Boolean = // change output to returning SemanticError
        if (varTable.head.contains(name)) { // Already declared in current scope
            varTable.head(name) = typ
            false
        }
        else {
            varTable.head(name) = typ
            true
        }

    // Looks up a symbol from innermost to outermost scope
    def lookupSymbol(name: String): Option[WACCType] = {
        for (scope <- varTable) {
            if (scope.contains(name)) {
                scope(name)
            }
        }
        None
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

case class FunctionSignature(returnType: WACCType, paramTypes: List[WACCType])
