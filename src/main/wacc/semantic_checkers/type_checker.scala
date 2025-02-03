package semantic_checkers

import AST.types.*
import AST.statements.*
import AST.expressions.*
import parsley.Result
import parsley.Success

object type_checker {
    val defaultPos: (Int, Int) = (-1, -1)

    def invariant(t1: WACCType, t2: WACCType): Boolean = 
        t1 != ArrayType(CharType()(t1.pos))(t1.pos) && t2 != StringType()(t2.pos)

    def weakened(t1: WACCType, t2: WACCType): Boolean = (t1, t2) match
            case (ArrayType(CharType()), StringType()) => true
            case (NonErasedPairType(_, _), ErasedPairType()) => true
            case (ErasedPairType(), NonErasedPairType(_, _)) => true
            case _ => false

    def compatible(t1: WACCType, t2: WACCType): Boolean = 
        t1 == t2 || weakened(t1, t2) || t1 == AnyType() || t2 == AnyType()

    def lca(a: WACCType, b: WACCType): Option[WACCType] = {
        if (compatible(a, b)) Some(b)
        else if (compatible(b, a)) Some(a)
        else None
    }

    def commonAncestor(ts: List[Option[WACCType]]): Option[WACCType] = {
        val definedTypes = ts.flatten
        definedTypes match {
            case Nil => None
            case head :: tail =>
            tail.foldLeft(Option(head)) { (currentAncestor, t) =>
                currentAncestor.flatMap(a => lca(a, t))
            }
        }
    }

    def getType(rVal: RValue)(implicit st: SymbolTable): Option[WACCType] = rVal match {
        
        case ArrayLiter(elements) =>
            commonAncestor(elements.map(getType))
        
        case NewPair(e1, e2) =>
            for {
                t1 <- getType(e1)
                t2 <- getType(e2)
            } yield NonErasedPairType(t1, t2)(defaultPos)
        
        case Call(Ident(name), _) =>
            st.lookupFunction(name).map(_.returnType)
    }

    def getType(lVal: LValue)(
        implicit st: SymbolTable
    ): Option[WACCType] = lVal match {
        case First(insideLVal) =>
            getType(insideLVal).collect {
                case NonErasedPairType(t, _) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
            }
        
        case Second(insideLVal) =>
            getType(insideLVal).collect {
                case NonErasedPairType(_, t) => t
                case ErasedPairType()        => UnknownType()(defaultPos)
            }
    }

    def getType(expr: Expr)(implicit st: SymbolTable): Option[WACCType] = expr match {
        // Literal cases
        case IntLiter(_)  => Some(IntType()(defaultPos))
        case BoolLiter(_) => Some(BoolType()(defaultPos))
        case CharLiter(_) => Some(CharType()(defaultPos))
        case StrLiter(_)  => Some(StringType()(defaultPos))
        case PairLiter()  =>
            Some(NonErasedPairType(AnyType()(defaultPos), AnyType()(defaultPos))(defaultPos))
        
        // Parentheses
        case Paren(e) =>
            getType(e)
        
        // Unary operators
        case Not(_)   => Some(BoolType()(defaultPos))
        case Negate(_) => Some(IntType()(defaultPos))
        case Len(_)   => Some(IntType()(defaultPos))
        case Ord(_)   => Some(IntType()(defaultPos))
        case Chr(_)   => Some(CharType()(defaultPos))
        
        // Binary operators producing Int results
        case Mul(_, _) | Div(_, _) | Mod(_, _) | Add(_, _) | Sub(_, _) =>
            Some(IntType()(defaultPos))
        
        // Binary operators producing Bool results
        case Less(_, _) | LessEqual(_, _) | Greater(_, _) | GreaterEqual(_, _) |
            Equal(_, _) | NotEqual(_, _) | And(_, _) | Or(_, _) =>
            Some(BoolType()(defaultPos))
        
        // Identifiers
        case Ident(name) =>
            st.lookupSymbol(name)
        
        // Array elements
        case ArrayElem(id, _) =>
            getType(id: Expr).collect { case ArrayType(t) => t }
    }

    def verifyUnary(expr: UnaryOp)(
        implicit st: SymbolTable
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
        implicit st: SymbolTable
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

    def verifyStmt(stmt: Stmt)(
        implicit st: SymbolTable
    ): Result[Error, Stmt] = stmt match {
        
        case Exit(e) => getType(e) match {
            case Some(IntType()) => Success(stmt)
            case _ => ???
        }
        case If(e, _, _) => getType(e) match {
            case Some(BoolType()) => Success(stmt)
            case _ => ???
        }
        case While(e, _) => getType(e) match {
            case Some(BoolType()) => Success(stmt)
            case _ => ???
        }
        case Print(e) => getType(e) match {
            case Some(_) => Success(stmt)
            case _ => ???
        }
        case Println(e) => getType(e) match {
            case Some(_) => Success(stmt)
            case _ => ???
        }
        case Read(e) => getType(e) match {
            case Some(_) => Success(stmt)
            case _ => ???
        }
        case Declare(t1, _, v) => getType(v) match {
            case Some(t2) =>
                if compatible(t1, t2) then Success(stmt) else ???
            case _ => ???
        }
        case Assign(v1, v2) => (getType(v1), getType(v2)) match {
            case (Some(t1), Some(t2)) =>
                if compatible(t1, t2) then Success(stmt) else ???
            case _ => ???
        }
    }
    
}