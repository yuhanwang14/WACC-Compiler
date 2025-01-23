package utils

import parsley.generic
import parsley.Parsley
import parsley.ap._
import parsley.position.pos

trait ParserSingletonBridgePos[+A] extends generic.ErrorBridge {
    protected def con(pos: (Int, Int)): A
    infix def from(op: Parsley[?]): Parsley[A] = error(pos.map(this.con(_)) <* op)
    final def <#(op: Parsley[?]): Parsley[A] = this from op
}

trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    def apply(x: A)(pos: (Int, Int)): B
    def apply(x: Parsley[A]): Parsley[B] = error(ap1(pos.map(con), x))

    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
}

trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonBridgePos[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C
    def apply(x: Parsley[A], y: =>Parsley[B]): Parsley[C] = error(ap2(pos.map(con), x, y))

    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
}