package wacc.semantics

import scala.collection.mutable

class SymbolTable {
    

    // private val scopes = mutable.Stack[mutable.Map[String, String]]()

    // def enterScope(): Unit = scopes.push(mutable.Map())
    // def exitScope(): Unit = scopes.pop()

    // def addSymbol[T](name: String, typ: String): Boolean = {
    //     if (scopes.top.contains(name)) false // Symbol already exists in this scope
    //     else {
    //         scopes.top(name) = typ
    //         true
    //     }
    // }

    // def lookupSymbol[T](name: String): Option[String] = {
    //     scopes.reverseIterator.flatMap(_.get(name)).nextOption()
    // }
}
