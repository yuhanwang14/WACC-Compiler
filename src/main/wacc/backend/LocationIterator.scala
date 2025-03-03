package backend

import instructions.{WRegister, XRegister}

class LocationIterator {

  val regSeq: Seq[Int] = (19 to 28) ++ (??? /* to implement arg registers */ ) ++ (10 to 15) :+ 18
  var at: Int = 0
  var stackOffset: Int = 0
  override def next(size: Int): Location =
    if at == regSeq.size then
      val retVal = stackOffset
      stackOffset += size
      retVal
    else
      val retVal = if size <= 4 then WRegister(at) else XRegister(at)
      at += 1
      retVal
}
