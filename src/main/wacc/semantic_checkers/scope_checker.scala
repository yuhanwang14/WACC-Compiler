package semantic_checkers

import AST.types.*
import AST.statements.*
import AST.expressions.*

object scopeChecker {
    def checkUpdate(s: Stmt)(implicit st: SymbolTable): Boolean = s match {
        case Declare(t, i, r) => decomposeRValue(r) && st.addSymbol(i.name, AnyType()(s.pos))
        case Assign(l, r) => decomposeRValue(r) && decomposeLValue(l)
        case _ => true
    }
    
    private def decomposeLValue(l: LValue)(implicit st: SymbolTable): Boolean = ???

    private def decomposeRValue(r: RValue)(implicit st: SymbolTable): Boolean = r match
        case e: Expr => decomposeExpr(e)
        case ArrayLiter(es) => decomposeExpr(es*)
        case NewPair(e1, e2) => decomposeExpr(e1, e2)
        case Call(i, argL) => decomposeExpr(i::argL.map(_.es).getOrElse(List.empty)*)

    def decomposeExpr(es: Expr*)(implicit st: SymbolTable): Boolean = es.forall match {
        case Or(x, y) => decomposeExpr(x, y)
        case And(x, y) => decomposeExpr(x, y)
        case Equal(x, y) => decomposeExpr(x, y)
        case NotEqual(x, y) => decomposeExpr(x, y)
        case Less(x, y) => decomposeExpr(x, y)
        case LessEqual(x, y) => decomposeExpr(x, y)
        case Greater(x, y) => decomposeExpr(x, y)
        case GreaterEqual(x, y) => decomposeExpr(x, y)

        case Add(x, y) => decomposeExpr(x, y)
        case Sub(x, y) => decomposeExpr(x, y)
        case Mul(x, y) => decomposeExpr(x, y)
        case Div(x, y) => decomposeExpr(x, y)
        case Mod(x, y) => decomposeExpr(x, y)

        case Not(x) => decomposeExpr(x)
        case Negate(x) => decomposeExpr(x)
        case Len(x) => decomposeExpr(x)
        case Ord(x) => decomposeExpr(x)
        case Chr(x) => decomposeExpr(x)
        
        case ArrayElem(ident, exprs) => decomposeExpr(ident::exprs*)
        case Paren(x) => decomposeExpr(x)
        
        case Ident(name) => st.lookupSymbol(name).isDefined

        case _ => true
    }
}
