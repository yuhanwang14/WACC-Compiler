package backend

import instructions.*
import scala.collection.mutable.{Set as MutableSet, Map as MutableMap}
import scala.math
import common.types.WaccType
import javax.imageio.spi.RegisterableService

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

class RegisterAllocator(
    vars: Seq[(String, WaccType)],
    inheritedMap: Iterable[(String, (Location, Int))]
)(implicit registerIterator: LocationIterator = LocationIterator()) {
    val varMap: MutableMap[String, (Location, Int)] = MutableMap.from(inheritedMap)
    
    def +:(vars: Seq[(String, WaccType)]): RegisterAllocator = RegisterAllocator(this.vars ++ vars, varMap)

    def apply(identifier: String): Location = ???
}
