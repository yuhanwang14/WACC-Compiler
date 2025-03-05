package ast

import common.types as commonType

object TypeBridge {
  def fromAst(astType: WaccType): commonType.WaccType = astType match {
    case ast.AnyType()                 => commonType.AnyType()
    case ast.IntType()                 => commonType.IntType()
    case ast.BoolType()                => commonType.BoolType()
    case ast.CharType()                => commonType.CharType()
    case ast.StringType()              => commonType.StringType()
    case ast.ArrayType(t1)             => commonType.ArrayType(fromAst(t1))
    case ast.ErasedPairType()          => commonType.PairType(commonType.AnyType(), commonType.AnyType())
    case ast.NonErasedPairType(t1, t2) => commonType.PairType(fromAst(t1), fromAst(t2))
    case _                             => commonType.UnknownType()
  }

  def unpackFunction(astFunc: ast.Func): (String, commonType.FunctionSignature) =
    (astFunc.ti._2.name, commonType.FunctionSignature(astFunc.ti._1, astFunc.ps.map(_.ps.map(_.t)).getOrElse(List())))
}
