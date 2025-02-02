package semantic_checkers

import AST.types.*
import AST.statements.*
import AST.expressions.*
import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable
import parsley.Result
import parsley.Success

object type_checker {
    val defaultPos: (Int, Int) = (-1, -1)
    def compatible(t1: WACCType, t2: WACCType): Boolean = 
        t1 == t2 || ((t1, t2) match {
            case (UnknownType(), _) => false
            case (_, UnknownType()) => false
            case (ArrayType(CharType()), StringType()) => true
            case (ArrayType(tt1), ArrayType(tt2)) => 
                compatible(tt1, tt2) && 
                tt1 != ArrayType(CharType()(defaultPos))(defaultPos) && 
                tt2 != StringType()(defaultPos)
            case (NonErasedPairType(t1_1, t1_2), NonErasedPairType(t2_1, t2_2)) => 
                compatible(t1_1, t2_1) && compatible(t1_2, t2_2) && 
                t1_1 != ArrayType(CharType()(defaultPos)) && t2_1 != StringType()(defaultPos) &&
                t1_2 != ArrayType(CharType()(defaultPos)) && t2_2 != StringType()(defaultPos)
            case (NonErasedPairType(_, _), ErasedPairType()) => true
            case (ErasedPairType(), NonErasedPairType(_, _)) => true    
            case (AnyType(), _) => true
            case (_, AnyType()) => true
            case _ => false
        })

    def commonAncestor(ts: List[Option[WACCType]]): Option[WACCType] = ts match {
    case Nil => None
    case headOpt :: tail =>
        headOpt match {
        case None => None 
        case Some(head) =>
            var resultType = head
            var valid = true
            breakable {
                tail.foreach {
                    case Some(t) =>
                        if (compatible(t, resultType)) {
                            resultType = t
                        }
                        else if (!compatible(resultType, t)) {
                            valid = false
                            break
                        }
                    case None => {
                        valid = false
                        break
                    }
                }
            }
            if (valid) Some(resultType) else None
        }
    }

    def getType(rVal: RValue)(
        implicit varTable: mutable.Stack[mutable.Map[String, WACCType]], 
        funcTable: mutable.Map[String, FunctionSignature]
    ): Option[WACCType] = rVal match {
        case (expr: Expr) => getType(expr)
        case ArrayLiter(es) => commonAncestor(es.map(getType))
        case NewPair(e1, e2) => {
            val tyOpt1 = getType(e1)
            val tyOpt2 = getType(e2)
            tyOpt1 match {
                case Some(ty1) => tyOpt2 match {
                    case Some(ty2) => Some(NonErasedPairType(ty1, ty2)(defaultPos))
                    case None => None
                }
                case None => None
            }
        }
        case First(insideLVal) => getType(insideLVal) match {
            case Some(NonErasedPairType(t, _)) => Some(t)
            case Some(ErasedPairType()) => Some(UnknownType()(defaultPos))
            case _ => None
        }
        case Second(insideLVal) => getType(insideLVal) match {
            case Some(NonErasedPairType(_, t)) => Some(t)
            case Some(ErasedPairType()) => Some(UnknownType()(defaultPos))
            case _ => None
        }
        case Call(Ident(name), _) => funcTable.get(name) match {
            case Some(funcSign) => Some(funcSign.returnType)
            case None => None
        }
    } 

    def getType(lVal: LValue)(
        implicit varTable: mutable.Stack[mutable.Map[String, WACCType]]
    ): Option[WACCType] = lVal match {
        case ArrayElem(id, _) => getType(id: Expr) match {
            case Some(ArrayType(t)) => Some(t)
            case _ => None
        }
        case Ident(name) => varTable.top.get(name)
        case First(insideLVal) => getType(insideLVal) match {
            case Some(NonErasedPairType(t, _)) => Some(t)
            case Some(ErasedPairType()) => Some(UnknownType()(defaultPos))
            case _ => None
        }
        case Second(insideLVal) => getType(insideLVal) match {
            case Some(NonErasedPairType(_, t)) => Some(t)
            case Some(ErasedPairType()) => Some(UnknownType()(defaultPos))
            case _ => None
        }
    }

    def getType(expr: Expr)(
        implicit varTable: mutable.Stack[mutable.Map[String, WACCType]]
    ): Option[WACCType] = expr match {
        case IntLiter(x) => Some(IntType()(defaultPos))
        case BoolLiter(x) => Some(BoolType()(defaultPos))
        case CharLiter(c) => Some(CharType()(defaultPos))
        case StrLiter(s) => Some(StringType()(defaultPos))
        case PairLiter() => Some(NonErasedPairType(AnyType()(defaultPos), AnyType()(defaultPos))(defaultPos))
        case Paren(e) => getType(e)
        case Not(e) => Some(BoolType()(defaultPos))
        case Negate(e) => Some(IntType()(defaultPos))
        case Len(e) => Some(IntType()(defaultPos))
        case Ord(e) => Some(IntType()(defaultPos))
        case Chr(e) => Some(CharType()(defaultPos))
        case Mul(e1, e2) => Some(IntType()(defaultPos))
        case Div(e1, e2) => Some(IntType()(defaultPos))
        case Mod(e1, e2) => Some(IntType()(defaultPos))
        case Add(e1, e2) => Some(IntType()(defaultPos))
        case Sub(e1, e2) => Some(IntType()(defaultPos))
        case Less(e1, e2) => Some(BoolType()(defaultPos))
        case LessEqual(e1, e2) => Some(BoolType()(defaultPos))
        case Greater(e1, e2) => Some(BoolType()(defaultPos))
        case GreaterEqual(e1, e2) => Some(BoolType()(defaultPos))
        case Equal(e1, e2) => Some(BoolType()(defaultPos))
        case NotEqual(e1, e2) => Some(BoolType()(defaultPos))
        case And(e1, e2) => Some(BoolType()(defaultPos))
        case Or(e1, e2) => Some(BoolType()(defaultPos))
        case Ident(name) => varTable.top.get(name)
        case ArrayElem(id, _) => getType(id: Expr) match {
            case Some(ArrayType(t)) => Some(t)
            case _ => None
        }
    }
    
    def verifyUnary(expr: UnaryOp)(
        implicit varTable: mutable.Stack[mutable.Map[String, WACCType]]
    ): Result[Error, Expr] = expr match {
        case Not(e) => getType(e) match {
            case Some(BoolType()) => Success(expr)
            case _ => ???
        }
        case Negate(e) => getType(e) match {
            case Some(IntType()) => Success(expr)
            case _ => ???
        }
        case Len(e) => getType(e) match {
            case Some(t) => 
                if (compatible(ArrayType(AnyType()(defaultPos))(defaultPos), t)) {
                    Success(expr)
                } else {
                    ???
                }
            case None => ???
        }
        case Ord(e) => getType(e) match {
            case Some(CharType()) => Success(expr)
            case _ => ???
        }
        case Chr(e) => getType(e) match {
            case Some(IntType()) => Success(expr)
            case _ => ???
        }
    }

    def verifyBinary(expr: BinaryOp)(
        implicit varTable: mutable.Stack[mutable.Map[String, WACCType]]
    ): Result[Error, Expr] = expr match {
        case Mul(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(IntType()), Some(IntType())) => Success(expr)
            case _ => ???
        }
        case Div(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(IntType()), Some(IntType())) => Success(expr)
            case _ => ???
        }
        case Mod(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(IntType()), Some(IntType())) => Success(expr)
            case _ => ???
        }
        case Add(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(IntType()), Some(IntType())) => Success(expr)
            case _ => ???
        }
        case Sub(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(IntType()), Some(IntType())) => Success(expr)
            case _ => ???
        }
        case Greater(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(IntType()), Some(IntType())) => Success(expr)
            case (Some(CharType()), Some(IntType())) => Success(expr)
            case (Some(IntType()), Some(CharType())) => Success(expr)
            case (Some(CharType()), Some(CharType())) => Success(expr)
            case _ => ???
        }
        case GreaterEqual(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(IntType()), Some(IntType())) => Success(expr)
            case (Some(CharType()), Some(IntType())) => Success(expr)
            case (Some(IntType()), Some(CharType())) => Success(expr)
            case (Some(CharType()), Some(CharType())) => Success(expr)
            case _ => ???
        }
        case Less(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(IntType()), Some(IntType())) => Success(expr)
            case (Some(CharType()), Some(IntType())) => Success(expr)
            case (Some(IntType()), Some(CharType())) => Success(expr)
            case (Some(CharType()), Some(CharType())) => Success(expr)
            case _ => ???
        }
        case LessEqual(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(IntType()), Some(IntType())) => Success(expr)
            case (Some(CharType()), Some(IntType())) => Success(expr)
            case (Some(IntType()), Some(CharType())) => Success(expr)
            case (Some(CharType()), Some(CharType())) => Success(expr)
            case _ => ???
        }
        case Equal(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(t1), Some(t2)) => {
                if (compatible(t1, t2) || compatible(t2, t1)) {
                    Success(expr)
                } else {
                    ???
                }
            }
            case _ => ???
        }
        case NotEqual(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(t1), Some(t2)) => {
                if (compatible(t1, t2) || compatible(t2, t1)) {
                    Success(expr)
                } else {
                    ???
                }
            }
            case _ => ???
        }
        case And(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(BoolType()), Some(BoolType())) => Success(expr)
            case _ => ???
        }
        case Or(e1, e2) => (getType(e1), getType(e2)) match {
            case (Some(BoolType()), Some(BoolType())) => Success(expr)
            case _ => ???
        }
    }
}