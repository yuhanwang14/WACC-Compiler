package semantic_checkers

import AST.statements.*

class SemanticError {}

object semantic_checker {
    def checker(prog: Program): Either[SemanticError, Unit] = {
        implicit val t = new SymbolTable()
        checkProgram(prog)
    }

    def checkProgram(prog: Program)(implicit t: SymbolTable): Either[SemanticError, Unit] = {
        // 1. record all function
        println(t.getFuncTable())
        prog.fs.foreach(t.addFunction)

        println(t.getFuncTable())

        // 2. check functions
        prog.fs.foreach(checkFunction)

        // 3. check stmt
        checkStatement(prog.s)
    }

    def checkFunction(func: Func)(implicit t: SymbolTable): Either[SemanticError, Unit] = {
        t.enterScope()
        t.exitScope()
        Right(())
    }

    def checkStatement(stmt: Stmt)(implicit t: SymbolTable): Either[SemanticError, Unit] = {
        Right(())
    }
}
