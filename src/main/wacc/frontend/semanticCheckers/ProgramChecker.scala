package semanticCheckers

import ast.*
import errors.*
import scala.collection.mutable.ListBuffer
import common.SymbolTable

object ProgramChecker {

  def check(prog: Program)(implicit
      lines: Seq[String],
      source: String
  ): ListBuffer[Error] = {
    implicit val symbolTable = new SymbolTable()
    implicit val errors: ListBuffer[Error] = ListBuffer()
    symbolTable.enterScope()
    prog.fs.foreach { f =>
      if (!symbolTable.addFunction(f)) {
        errors +=
          ErrorBuilder.specializedError(
            Seq(
              s"Function redefinition error: illegal redefinition of function ${f.ti._2.name} "
            ),
            f.pos
          )
      }
    }
    prog.fs.foreach(checkFunction(_))
    symbolTable.exitScope()
    SemanticChecker.verifyStmt(prog.s)
    errors
  }

  private def checkFunction(f: Func)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): Unit = {
    st.enterFunctionScope(f.ti._2.name)

    // add params to symbol table
    f.ps match {
      case Some(ParamList(params)) =>
        params.foreach { (p: Param) =>
          if (!st.addSymbol(p.i.name, p.t)) {
            errors +=
              ErrorBuilder.specializedError(
                Seq(
                  s"Scope error: illegal redeclaration of variable ${p.i.name} "
                ),
                p.pos
              )
          }
        }
      case _ =>
    }

    SemanticChecker.verifyStmt(f.s)
    st.exitScope()
  }

}
