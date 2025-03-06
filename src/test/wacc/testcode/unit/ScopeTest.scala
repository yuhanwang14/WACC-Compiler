import org.scalatest.funsuite.AnyFunSuite

import common.{Scope, GlobalScope, FunctionScope}
import frontend.ast.{boolType, charType, intType, stringType, pairType, arrayType}

class ScopeTest extends AnyFunSuite:
  def getGlobalScope = GlobalScope()
  def getFuncScope1 = FunctionScope("foo", Some(intType), Nil)
  def getFuncScope2 = FunctionScope(
    "bar",
    Some(intType),
    Seq("param1" -> arrayType(intType), "param2" -> pairType(stringType, boolType))
  )

  test("Can add multiple child scopes to global scope."):
    val globalScope = getGlobalScope
    val childScope1 = globalScope.addChild()
    val childScope2 = globalScope.addChild()
    val _ = childScope2.addChild()
    val _ = childScope2.addChild()
    val _ = childScope2.addChild()
    assert(globalScope.children.size == 2)
    assert(childScope1.children.size == 0)
    assert(childScope2.children.size == 3)

  test("Variables in parent scope can be passed to children."):
    val globalScope = getGlobalScope
    val childScope1 = globalScope.addChild()
    childScope1.addSymbol("iVal", intType)
    childScope1.addSymbol("cVal", charType)
    val childScope2 = childScope1.addChild()
    childScope2.addSymbol("cVal1", charType)
    val childScope3 = childScope2.addChild()
    childScope2.addSymbol("cVal2", charType)
    val childScope4 = childScope2.addChild()
    assert(childScope3.lookupSymbol("iVal") == Some(intType))
    assert(childScope3.lookupSymbol("cVal") == Some(charType))
    assert(childScope3.lookupSymbol("cVal1") == Some(charType))
    assert(childScope3.lookupSymbol("cVal2") == None)
    assert(childScope4.lookupSymbol("iVal") == Some(intType))
    assert(childScope4.lookupSymbol("cVal1") == Some(charType))
    assert(childScope4.lookupSymbol("cVal2") == Some(charType))

  test("Variable defined in outer scope can be shadowed"):
    val globalScope = getGlobalScope
    val childScope1 = globalScope.addChild()
    childScope1.addSymbol("val", intType)
    val childScope2 = childScope1.addChild()
    childScope2.addSymbol("val", boolType)
    assert(childScope1.lookupSymbol("val") == Some(intType))
    assert(childScope2.lookupSymbol("val") == Some(boolType))
    assert(childScope1.unshadow("val") != childScope2.unshadow("val"))

  test("Parameters is accessible from nested scopes or shadowed."):
    val funcScope = getFuncScope2
    val funcChildScope1 = funcScope.addChild()
    val funcChildScope2 = funcChildScope1.addChild()
    funcChildScope2.addSymbol("param1", stringType)
    val funcChildScope3 = funcChildScope1.addChild()
    assert(funcChildScope1.lookupSymbol("param1") == Some(arrayType(intType)))
    assert(funcChildScope2.lookupSymbol("param1") == Some(stringType))
    assert(funcChildScope3.lookupSymbol("param1") == Some(arrayType(intType)))
    val prefixedId1 = funcChildScope1.unshadow("param1")
    funcChildScope1.addSymbol("param1", boolType)
    assert(funcChildScope1.lookupSymbol("param1") == Some(boolType))
    assert(funcChildScope1.unshadow("param1") != prefixedId1)

