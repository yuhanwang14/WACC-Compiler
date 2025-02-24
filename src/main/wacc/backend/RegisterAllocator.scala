import instructions.*
import scala.collection.mutable.{Map, ListBuffer}

/* aarch64 convention for local variables:

   Register x19-x28: general purposes (saved by callee)
   Register x0-x7, x9-x15: general purposes (saved by caller)
   Register x18: platform register, not reserved by linux (saved by caller)
   Any extra variables should be stored on stack
 */

case class Variable(location: Either[Register, Int], size: Int)

class RegisterAllocator {

    type Offset = Int
    type Location = Either[Register, Offset]
    private val varMap: Map[String, Variable] = Map.empty[String, Variable]
    private var availableRegisters: List[Register] 
        = ((19 to 28) ++ (0 to 7) ++ (9 to 15)).toList.map(n => XRegister(n))
    private var offset = 0

    private val CalleeRegister: ListBuffer[Register] = ListBuffer.empty[Register]
    private val CallerRegister: ListBuffer[Register] = ListBuffer.empty[Register]

    private def addRegister(reg: Register): Unit = {
        if (19 <= reg.number && reg.number <= 28)
            CalleeRegister += reg
        else
            CallerRegister += reg
    }

    def allocate(name: String, size: Int): Variable = {
        val variable = if (availableRegisters.nonEmpty) {
            val reg = availableRegisters.head
            availableRegisters = availableRegisters.tail
            addRegister(reg)
            Variable(Left(reg), size)
        } else {
            offset -= size
            Variable(Right(offset), size)
        }
        varMap(name) = variable
        variable
    }
}