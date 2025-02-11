import ast.*
import semanticCheckers.SymbolTable
import scala.collection.mutable.ListBuffer

object generator {

    def generate(prog: Program)(implicit
        symbolTable: SymbolTable,
        asmLine: ListBuffer[StringBuilder]
    ): Unit = {
        generateBlock(prog.s)
        prog.fs.foreach(generateFunc(_))
        ???
    }

    private def generateBlock(block: Stmt)(implicit
        symbolTable: SymbolTable,
        asmLine: ListBuffer[StringBuilder]): Unit = {
        
        val stmts: List[Stmt] = block match {
            case Block(sts) => sts
            case _ => return
        }
        stmts.foreach(generateStmt(_))
        
    }

    private def generateStmt(stmt: Stmt)(implicit
        symbolTable: SymbolTable,
        asmLine: ListBuffer[StringBuilder]): Unit = {
        ???
    }

    private def generateFunc(func: Func)(implicit
        symbolTable: SymbolTable,
        asmLine: ListBuffer[StringBuilder]): Unit = {
        ???
    }
    

}