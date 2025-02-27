package common.types

case class FunctionSignature(returnType: ast.WaccType, paramTypes: Seq[ast.WaccType])
