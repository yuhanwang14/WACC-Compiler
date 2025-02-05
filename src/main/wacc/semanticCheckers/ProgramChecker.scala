package semanticCheckers

import ast.*
import errors.Error
import scala.collection.mutable.ListBuffer

object ProgramChecker {

    def check(prog: Program)(
        implicit lines: Seq[String],
        source: String
    ): ListBuffer[Error] = {
        implicit val symbolTable = new SymbolTable()
        implicit val errors: ListBuffer[Error] = ListBuffer() 
        symbolTable.enterScope()
        prog.fs.forall(f => symbolTable.addFunction(f))
        prog.fs.foreach(checkFunction(_))
        symbolTable.exitScope()
        SemanticChecker.verifyStmt(prog.s)
        errors
    }

    private def checkFunction(f: Func)(
        implicit st: SymbolTable,
        errors: ListBuffer[Error],
        lines: Seq[String],
        source: String
    ): Unit = {
        st.setGlobalScope(false)
        st.setReturnType(f.t)
        st.enterScope()
        
        // add params to symbol table
        f.ps match {
            case Some(ParamList(params)) => 
                params.foreach((p: Param) => st.addSymbol(p.i.name, p.t))
            case _ => 
        }

        SemanticChecker.verifyStmt(f.s)
        st.exitScope()
        st.clearReturnType()
        st.setGlobalScope(true)
    }
        
}
