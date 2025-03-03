package backend

import instructions.*
import scala.collection.mutable.{Map, ArrayBuffer}
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

case class Variable(location: Either[Register, Int], size: Int)

class RegisterAllocatorOld(
    numOfVariables: Int = 10,
    numOfParams: Int = 0
) {
    
    type Offset = Int
    type Location = Either[Register, Offset]
    val varMap: Map[String, Variable] = Map.empty[String, Variable]
    var availableRegisters: List[Register] 
        = ((19 to 28) ++ (numOfParams to 7) ++ (10 to 15) :+ 18).toList.map(n => XRegister(n))
    
    var varOffset = 0
    // start after stored fp, lr and registers about to use
    var paramOffset = 16 + 16 * math.floorDiv(math.min(numOfVariables, 10) + 1, 2)
    var currentParamRegister = 0

    val calleeRegister: ArrayBuffer[Register] = ArrayBuffer.empty[Register]
    val callerRegister: ArrayBuffer[Register] = ArrayBuffer.empty[Register]

    private def addRegister(reg: Register): Unit = {
        if (19 <= reg.number && reg.number <= 28)
            calleeRegister += reg
        else
            callerRegister += reg
    }

    def addParam(name: String, size: Int): Variable = {
        val variable: Variable = if (currentParamRegister < 8) {
            val reg = XRegister(currentParamRegister)
            currentParamRegister += 1
            callerRegister += reg
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

    def getLocation(name: String): Either[Register, Int] = {
        varMap.get(name).fold(Right(0))(x => x.location)
    }

    override def clone(): RegisterAllocator = {
        val cloned = RegisterAllocator(numOfVariables, numOfParams)
        cloned.calleeRegister.addAll(calleeRegister)
        cloned.callerRegister.addAll(callerRegister)
        cloned.availableRegisters = availableRegisters
        cloned.varMap.addAll(varMap)
        cloned.paramOffset = paramOffset
        cloned.varOffset = varOffset
        cloned.currentParamRegister = currentParamRegister
        cloned
    }
}
