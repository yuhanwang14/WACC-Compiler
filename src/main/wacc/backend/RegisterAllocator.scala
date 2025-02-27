package backend

import instructions.*
import scala.collection.mutable.{Map, ListBuffer}
import scala.math

/* aarch64 convention for registers:

   Register x19-x28: general purposes (saved by callee)
   Register x0-x7, x10-x15: general purposes (saved by caller)
   Register x18: platform register, not reserved by linux (saved by caller)
   Any extra variables should be stored on stack

   x8, x9 is reserved to store to and load from memory

   Location is stored as an offset with respect to frame pointer (fp), postive 
   offset to retrieve function params and negative offset to retrieve local variables
 */

class RegisterAllocator(
    numOfVariables: Int,
    numOfParams: Int
) {

    case class Variable(location: Either[Register, Int], size: Int)
    
    type Offset = Int
    type Location = Either[Register, Offset]
    private val varMap: Map[String, Variable] = Map.empty[String, Variable]
    private var availableRegisters: List[Register] 
        = ((19 to 28) ++ (numOfParams to 7) ++ (10 to 15)).toList.map(n => XRegister(n))
    
    private var varOffset = 0
    // start after stored fp, lr and registers about to use
    private var paramOffset = 16 + 16 * math.floorDiv(math.max(numOfVariables, 10) + 1, 2)
    private var currentParamRegister = 0

    private val CalleeRegister: ListBuffer[Register] = ListBuffer.empty[Register]
    private val CallerRegister: ListBuffer[Register] = ListBuffer.empty[Register]

    private def addRegister(reg: Register): Unit = {
        if (19 <= reg.number && reg.number <= 28)
            CalleeRegister += reg
        else
            CallerRegister += reg
    }

    def addParam(name: String, size: Int): Variable = {
        val variable: Variable = if (currentParamRegister < 8) {
            val reg = XRegister(currentParamRegister)
            currentParamRegister += 1
            Variable(Left(reg), size)
        } else {
            paramOffset += size
            Variable(Right(paramOffset - size), size)
        }
        varMap(name) = variable
        variable
    }

    def allocate(name: String, size: Int): Variable = {
        val variable = if (availableRegisters.nonEmpty) {
            val reg = availableRegisters.head
            availableRegisters = availableRegisters.tail
            addRegister(reg)
            Variable(Left(reg), size)
        } else {
            varOffset -= size
            Variable(Right(varOffset), size)
        }
        varMap(name) = variable
        variable
    }
}