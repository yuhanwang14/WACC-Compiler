package common

import types.FunctionSignature

class FrozenSymbolTable private[common] (
    funcs: Iterable[(String, (FunctionSignature, FunctionScope))],
    globalScope: GlobalScope
):
  private val funcTable: Map[String, (FunctionSignature, FunctionScope)] = Map.from(funcs)
  def getFuncSignature(identifier: String): FunctionSignature = funcTable(identifier)._1
  def getFuncScope(identifier: String): FunctionScope = funcTable(identifier)._2
  val mainScope: Scope = globalScope.child
