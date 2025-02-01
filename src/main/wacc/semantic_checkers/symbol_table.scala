package wacc.semantics

import scala.collection.mutable
import AST.types.*
import AST.statements.Func

//  scopes are introduced by ‘begin .. end’, functions, if-statements, and while-loops

class SymbolTable {
    // Initialize with a global scope
    private val varTable = mutable.Stack[mutable.Map[String, WACCType]](mutable.Map())
    private val funcTable = mutable.Map[String, FunctionSignature]() 

    def getVarTable() = varTable

    def getFuncTable() = funcTable

    // Enters a new nested scope 
    def enterScope(): Unit = varTable.push(mutable.Map())

    // Exits the current scope but ensures the global scope is never removed
    def exitScope(): Unit = {
        if (varTable.size > 1) varTable.pop()
        else throw new RuntimeException("Cannot exit global scope")
    }

    // Adds a symbol to the current scope
    def addSymbol(name: String, typ: WACCType): Unit = {
        if (varTable.head.contains(name)) { // Already declared in current scope
            // error
            return
        }  
        else if (lookupSymbol(name).isDefined) { // Shadowing an existing variable
            // Shadowing
            return
        }
          
        varTable.head(name) = typ
    }

    // Looks up a symbol from innermost to outermost scope
    def lookupSymbol(name: String): Option[WACCType] = {
        varTable.find(_.contains(name)).flatMap(_.get(name))
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
