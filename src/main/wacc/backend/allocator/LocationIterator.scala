package backend.allocator

import backend.instructions.{WRegister, XRegister}

class LocationIterator(paramCount: Int):
  private val calleeRegs: Seq[Int] = (19 to 28)
  private val callerRegs: Seq[Int] = (paramCount to 7) ++ (10 to 15) :+ 18
  private val regs = calleeRegs ++ callerRegs

  var at: Int = 0
  var stackOffset: Int = 0

  def next(size: Int): Location =
    if at == calleeRegs.size + callerRegs.size then
      val retVal = stackOffset
      stackOffset += size
      retVal
    else
      val retVal = if size <= 4 then WRegister(regs(at)) else XRegister(regs(at))
      at += 1
      retVal

  
  def usedCallerRegisters: Seq[Int] = callerRegs.take(at - calleeRegs.size)
