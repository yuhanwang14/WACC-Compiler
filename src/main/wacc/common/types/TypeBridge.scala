package common.types

object TypeBridge {
  def fromAst(astType: ast.WaccType): WaccType = astType match {
    case ast.AnyType()                 => AnyType()
    case ast.IntType()                 => IntType()
    case ast.BoolType()                => BoolType()
    case ast.CharType()                => CharType()
    case ast.StringType()              => StringType()
    case ast.ArrayType(t1)             => ArrayType(fromAst(t1))
    case ast.ErasedPairType()          => PairType(AnyType(), AnyType())
    case ast.NonErasedPairType(t1, t2) => PairType(fromAst(t1), fromAst(t2))
    case _                             => UnknownType()
  }

  def unpackFunction(astFunc: ast.Func): (String, FunctionSignature) =
    (astFunc.ti._2.name, FunctionSignature(astFunc.ti._1, astFunc.ps.map(_.ps.map(_.t)).getOrElse(List())))
}
