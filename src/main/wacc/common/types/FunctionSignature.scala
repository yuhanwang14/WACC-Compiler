package common.types

import frontend.ast

case class FunctionSignature(returnType: ast.WaccType, paramTypes: Seq[ast.WaccType])
