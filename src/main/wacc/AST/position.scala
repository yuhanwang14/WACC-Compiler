package AST

object position {

    abstract trait Position {
        val pos: (Int, Int)
    }

}