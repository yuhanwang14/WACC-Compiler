package backend.allocator

import common.{Scope, FunctionScope}

extension (s: Scope)
  def maxConcurrentVars: Int =
    val paramCount = s match
        case fs: FunctionScope => fs.params.size
        case _                 => 0
    s.children.map(_.maxConcurrentVars).maxOption.getOrElse(s.varTable.size) - paramCount
      
