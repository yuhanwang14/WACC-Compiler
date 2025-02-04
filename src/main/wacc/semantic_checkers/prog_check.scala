package semantic_checkers

import ast.statements.*
import errors.errors.Error
import semanticChecker.*
import scala.collection.mutable.ListBuffer

class SemanticError {}

object semantic_checker {

    def checker(prog: Program)(
        implicit lines: Seq[String],
        sourceName: String
    ): ListBuffer[Error] = {
        implicit val symbolTable = new SymbolTable()
        implicit val errors: ListBuffer[Error] = ListBuffer() 
        symbolTable.enterScope()
        prog.fs.foreach(checkFunction(_))
        verifyStmt(prog.s)
        errors
    }

    def checkFunction(f: Func)(
        implicit st: SymbolTable,
        errors: ListBuffer[Error],
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
