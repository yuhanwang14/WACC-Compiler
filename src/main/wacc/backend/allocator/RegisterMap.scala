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

trait GenericRegisterMap:
  def apply(identifier: String): (Location, Int)
  def <~(vars: Iterable[(String, WaccType)]): GenericRegisterMap
  def usedCallerRegisters: Seq[Int]
  def stackOffset: Int
  def savingRegs(regToOffsets: Iterable[(Int, Int)]): CallerSavedRegisterMap

class RegisterMap private (
    vars: Iterable[(String, WaccType)],
    inheritedMap: Iterable[(String, (Location, Int))]
)(implicit locationIterator: LocationIterator)
    extends GenericRegisterMap:
  val varMap: MutableMap[String, (Location, Int)] = MutableMap.from(inheritedMap)
  vars.foreach((name, t) => varMap(name) = (locationIterator.next(t.byteSize), t.byteSize))

  def this(params: Iterable[(String, WaccType)], calleeRegisterCount: Int) =
    this(
      Nil,
      RegisterMap.mapParams(params, (calleeRegisterCount + 1) / 2 * 16)
    )(LocationIterator(params.size))

  def <~(vars: Iterable[(String, WaccType)]): GenericRegisterMap = RegisterMap(vars, varMap)

  def savingRegs(regToOffsets: Iterable[(Int, Int)]): CallerSavedRegisterMap =
    CallerSavedRegisterMap(this, Map.from(regToOffsets))

  export varMap.apply

  export locationIterator.usedCallerRegisters
  export locationIterator.stackOffset

class CallerSavedRegisterMap(original: RegisterMap, savedRegs: Map[Int, Int])
    extends GenericRegisterMap:
  val usedCallerRegisters: Seq[Int] = Nil
  export original.<~
  export original.stackOffset
  def apply(identifier: String): (Location, Int) =
    original.apply(identifier) match
      case (reg: Register, size) => (savedRegs.get(reg.number).getOrElse(reg), size)
      case loc                   => loc
  def savingRegs(regToOffsets: Iterable[(Int, Int)]): CallerSavedRegisterMap = throw Exception()

object RegisterMap:
  private def mapParams(
      params: Iterable[(String, WaccType)],
      start: Int
  ): Iterable[(String, (Location, Int))] =
    val (regParams, stackParams) = params.splitAt(8)
    regParams.zipWithIndex
      .map:
        case ((id, t), i) =>
          (id, (if t.byteSize > 4 then XRegister(i) else WRegister(i), t.byteSize))
      ++
        (if !stackParams.isEmpty then
           stackParams.tail
             .foldRight(
               stackParams.head match
                 case (id, t) => List((id, (start: Location) -> t.byteSize)) -> t.byteSize
             ) { case ((id, t), (unpacked, offset)) =>
               (unpacked :+ (id, offset -> t.byteSize)) -> (offset + t.byteSize)
             }
             ._1
         else Nil)
