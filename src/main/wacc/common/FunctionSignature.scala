package common

import ast.WaccType

case class FunctionSignature(returnType: WaccType, paramTypes: List[WaccType])
