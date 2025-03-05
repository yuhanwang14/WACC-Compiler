package semanticCheckers

import ast.*
import errors.*
import scala.collection.mutable.{ListBuffer, Set}
import common.SymbolTable

object ProgramChecker {

  def check(prog: Program)(implicit
      lines: Seq[String],
      source: String
  ): Either[ListBuffer[Error], (Program, SymbolTable)] = {
    implicit val symbolTable = new SymbolTable()
    implicit val errors: ListBuffer[Error] = ListBuffer()
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
    val newFuncs = prog.fs.map(checkFunction(_))
    symbolTable.exitToGlobalScope()
    val newStmt = SemanticChecker.verifyStmt(prog.s)
    return if errors.isEmpty then Right((Program(newFuncs, newStmt)(prog.pos), symbolTable))
    else Left(errors)
  }

  private def checkFunction(f: Func)(implicit
      st: SymbolTable,
      errors: ListBuffer[Error],
      lines: Seq[String],
      source: String
  ): Func = {
    st.enterFunctionScope(f.ti._2.name)

    // add params to symbol table
    f.ps match {
      case Some(ParamList(params)) =>
        val paramSet: Set[(String, WaccType)] = Set()
        params.foreach { (p: Param) =>
          if (!paramSet.add((p.i.name, p.t))) {
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

    val newStmt = SemanticChecker.verifyStmt(f.s)
    Func(f.ti, f.ps, newStmt)(f.pos)
  }

}
