package semantic_checkers

import ast.statements.*
import errors.errors.Error
import semanticChecker.*

class SemanticError {}

object semantic_checker {

    def checker(prog: Program)(
        implicit lines: Seq[String],
        source: String
    ): Unit = {
        implicit val symbolTable = new SymbolTable()
        implicit val errors: Seq[Error] = Seq()
        symbolTable.enterScope()
        prog.fs.foreach(checkFunction(_))
        verifyStmt(prog.s)
    }

    def checkFunction(f: Func)(
        implicit st: SymbolTable,
        errors: Seq[Error],
        lines: Seq[String],
        source: String
    ): Unit = {
        st.setReturnType(f.t)
        st.enterScope()
        
        // add params to symbol table
        f.ps match {
            case Some(ParamList(params)) => 
                params.foreach((p: Param) => st.addSymbol(p.i.name, p.t))
            case _ => 
        }

        verifyStmt(f.s)
        st.exitScope()
        st.addFunction(f)
    }
        
}
