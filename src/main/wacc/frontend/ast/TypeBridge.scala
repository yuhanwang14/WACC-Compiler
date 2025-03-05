package frontend.ast

import common.types as commonType

object TypeBridge {
  def fromAst(astType: WaccType): commonType.WaccType = astType match {
    case AnyType()                 => commonType.AnyType()
    case IntType()                 => commonType.IntType()
    case BoolType()                => commonType.BoolType()
    case CharType()                => commonType.CharType()
    case StringType()              => commonType.StringType()
    case ArrayType(t1)             => commonType.ArrayType(fromAst(t1))
    case ErasedPairType()          => commonType.PairType(commonType.AnyType(), commonType.AnyType())
    case NonErasedPairType(t1, t2) => commonType.PairType(fromAst(t1), fromAst(t2))
    case _                             => commonType.UnknownType()
  }

  def unpackFunction(astFunc: Func): (String, commonType.FunctionSignature) =
    (astFunc.ti._2.name, commonType.FunctionSignature(astFunc.ti._1, astFunc.ps.map(_.ps.map(_.t)).getOrElse(List())))
}
