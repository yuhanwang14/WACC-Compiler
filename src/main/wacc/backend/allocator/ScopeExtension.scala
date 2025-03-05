package backend.allocator

import common.Scope

extension (s: Scope)
  def maxConcurrentVars: Int =
    s.children.map(_.maxConcurrentVars).maxOption.getOrElse(s.varTable.size)
