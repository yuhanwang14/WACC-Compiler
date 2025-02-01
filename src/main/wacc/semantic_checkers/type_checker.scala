// package semantic_checker

// import AST.types.*
// import AST.statements.*
// import AST.expressions.*
// import scala.util.control.Breaks.{break, breakable}

// object type_checker {
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

//     def getType(expr: Expr): WACCType = expr match {
//         case IntLiter(x) => IntType
//         case BoolLiter(x) => BoolType
//         case CharLiter(c) => CharType
//         case StrLiter(s) => StringType
//         case PairLiter => NonErasedPairType(AnyType, AnyType)

//     }
    
    
//     def verifyType(expr: Expr): Option[Expr] = expr match {
//         case Or(e1, e2) => ???
//     }
// }