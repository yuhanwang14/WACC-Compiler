// package semantic_checker

// import AST.types.*
// import AST.statements.*
// import AST.expressions.*
// import scala.util.control.Breaks.{break, breakable}
// import scala.collection.mutable

// object type_checker {
//     val defaultPos: (Int, Int) = (-1, -1)
//     def compatible(t1: WACCType, t2: WACCType): Boolean = 
//         t1 == t2 || ((t1, t2) match {
//             case (ArrayType(CharType), StringType) => true
//             case (ArrayType(tt1), ArrayType(tt2)) => 
//                 compatible(tt1, tt2) && tt1 != ArrayType(CharType) && tt2 != StringType
//             case (NonErasedPairType(t1_1, t1_2), NonErasedPairType(t2_1, t2_2)) => 
//                 compatible(t1_1, t2_1) && compatible(t1_2, t2_2) && 
//                 t1_1 != ArrayType(CharType) && t2_1 != StringType &&
//                 t1_2 != ArrayType(CharType) && t2_2 != StringType
//             case (NonErasedPairType(_, _), ErasedPairType) => true
//             case (ErasedPairType, NonErasedPairType(_, _)) => true    
//             case (AnyType, _) => true
//             case (_, AnyType) => true
//             case _ => false
//         })

//     def commonAncestor(ts: List[WACCType]): Option[WACCType] = ts match {
//         case Nil => None
//         case head :: tail =>
//             var resultType = head
//             var valid = true
//             breakable {
//                 tail.foreach { t =>
//                     if (compatible(t, resultType))
//                         resultType = t
//                     else if (!compatible(resultType, t)) {
//                         valid = false
//                         break()
//                     }
//                 }
//             }
//             if (valid) Some(resultType) else None
//         }

//     def getType(lVal: LValue)(
//         implicit varTable: mutable.Stack[mutable.Map[String, WACCType]]
//     ): Option[WACCType] = lVal match {
//         case ArrayElem(id, _) => getType(id: Expr) match {
//             case Some(ArrayType(t)) => Some(t)
//             case _ => None
//         }
//         case Ident(name) => varTable.top.get(name)
//         case PairElem(insideLVal) => getType(insideLVal: LValue) {
//             case Some(NonErasedPairType())
//         }
//     }

//     def getType(expr: Expr)(
//         implicit varTable: mutable.Stack[mutable.Map[String, WACCType]]
//     ): Option[WACCType] = expr match {
//         case IntLiter(x) => Some(IntType)
//         case BoolLiter(x) => Some(BoolType)
//         case CharLiter(c) => Some(CharType)
//         case StrLiter(s) => Some(StringType)
//         case PairLiter => Some(NonErasedPairType(AnyType, AnyType)(defaultPos))
//         case Paren(e) => getType(e)
//         case Not(e) => Some(BoolType)
//         case Negate(e) => Some(IntType)
//         case Len(e) => Some(IntType)
//         case Ord(e) => Some(IntType)
//         case Chr(e) => Some(CharType)
//         case Mul(e1, e2) => Some(IntType)
//         case Div(e1, e2) => Some(IntType)
//         case Mod(e1, e2) => Some(IntType)
//         case Add(e1, e2) => Some(IntType)
//         case Sub(e1, e2) => Some(IntType)
//         case Less(e1, e2) => Some(BoolType)
//         case LessEqual(e1, e2) => Some(BoolType)
//         case Greater(e1, e2) => Some(BoolType)
//         case GreaterEqual(e1, e2) => Some(BoolType)
//         case Equal(e1, e2) => Some(BoolType)
//         case NotEqual(e1, e2) => Some(BoolType)
//         case And(e1, e2) => Some(BoolType)
//         case Or(e1, e2) => Some(BoolType)
//         case Ident(name) => varTable.top.get(name)
//         case ArrayElem(id, _) => getType(id: Expr) match {
//             case Some(ArrayType(t)) => Some(t)
//             case _ => None
//         }
//     }
    
    
//     def verifyType(expr: Expr): Option[Expr] = expr match {
//         case Or(e1, e2) => ???
//     }
// }