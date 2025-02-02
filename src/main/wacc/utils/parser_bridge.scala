package utils

import parsley.generic
import parsley.Parsley
import parsley.ap._
import parsley.position.pos

trait ParserSingletonBridgePos[+R] extends generic.ErrorBridge {
    protected def con(pos: (Int, Int)): R
    infix def from(op: Parsley[?]): Parsley[R] = error(pos.map(this.con(_)) <* op)
    final def <#(op: Parsley[?]): Parsley[R] = this from op
}

trait ParserBridgePos0[+R] extends ParserSingletonBridgePos[R] {
    def apply()(pos: (Int, Int)): R

    override final def con(pos: (Int, Int)): R = this.apply()(pos)
}

trait ParserBridgePos1[-T1, +R] extends ParserSingletonBridgePos[T1 => R] {
    def apply(t1: T1)(pos: (Int, Int)): R
    def apply(t1: Parsley[T1]): Parsley[R] = error(ap1(pos.map(con), t1))

    override final def con(pos: (Int, Int)): T1 => R = this.apply(_)(pos)
}

trait ParserBridgePos2[-T1, -T2, +R] extends ParserSingletonBridgePos[(T1, T2) => R] {
    def apply(t1: T1, t2: T2)(pos: (Int, Int)): R
    def apply(t1: Parsley[T1], t2: =>Parsley[T2]): Parsley[R] = error(ap2(pos.map(con), t1, t2))

    override final def con(pos: (Int, Int)): (T1, T2) => R = this.apply(_, _)(pos)
}

trait ParserBridgePos3[-T1, -T2, -T3, +R] extends ParserSingletonBridgePos[(T1, T2, T3) => R] {
    def apply(t1: T1, t2: T2, t3: T3)(pos: (Int, Int)): R
    def apply(t1: Parsley[T1], t2: =>Parsley[T2], t3: =>Parsley[T3]): Parsley[R] = 
        error(ap3(pos.map(con), t1, t2, t3))

    override final def con(pos: (Int, Int)): (T1, T2, T3) => R = this.apply(_, _, _)(pos)
}

trait ParserBridgePos4[-T1, -T2, -T3, -T4, +R] extends ParserSingletonBridgePos[(T1, T2, T3, T4) => R] {
    def apply(t1: T1, t2: T2, t3: T3, t4: T4)(pos: (Int, Int)): R
    def apply(t1: Parsley[T1], t2: =>Parsley[T2], t3: =>Parsley[T3], t4: =>Parsley[T4]): Parsley[R] = 
        error(ap4(pos.map(con), t1, t2, t3, t4))

    override final def con(pos: (Int, Int)): (T1, T2, T3, T4) => R = this.apply(_, _, _, _)(pos)
}