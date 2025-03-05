package backend.allocator

import backend.instructions.*
import scala.collection.mutable.{Map as MutableMap}
import common.types.WaccType

/* aarch64 convention for registers:

   Register x19-x28: general purposes (saved by callee)
   Register x0-x7, x10-x15: general purposes (saved by caller)
   Register x18: platform register, not reserved by linux (saved by caller)
   Any extra variables should be stored on stack

   x8, x9 is reserved to store to and load from memory

   Location is stored as an offset with respect to frame pointer (fp), postive
   offset to retrieve function params and negative offset to retrieve local variables
 */

type Location = Register | Int

class RegisterMap private (
    vars: Iterable[(String, WaccType)],
    inheritedMap: Iterable[(String, (Location, Int))]
)(implicit locationIterator: LocationIterator):
  val varMap: MutableMap[String, (Location, Int)] = MutableMap.from(inheritedMap)
  vars.foreach((name, t) => varMap(name) = (locationIterator.next(t.byteSize), t.byteSize))

  def this(params: Iterable[(String, WaccType)], calleeRegisterCount: Int) =
    this(
      Nil,
      if params.isEmpty then
        Nil
      else
        RegisterMap.mapParams(params, (calleeRegisterCount + 1) / 2 * 16)
    )(LocationIterator(params.size))

  def :+(vars: Iterable[(String, WaccType)]): RegisterMap = RegisterMap(vars, varMap)

  export varMap.apply

  export locationIterator.usedCallerRegisters
  export locationIterator.stackOffset

object RegisterMap:
  private def mapParams(params: Iterable[(String, WaccType)], start: Int): Iterable[(String, (Location, Int))] =
    params.tail
      .foldRight(
        params.head match
            case (id, t) => List((id, (start: Location) -> t.byteSize)) -> t.byteSize
      ) { case ((id, t), (unpacked, offset)) =>
        (unpacked :+ (id, offset -> t.byteSize)) -> (offset + t.byteSize)
      }
      ._1
