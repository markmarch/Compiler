package edu.nyu.compiler

import edu.nyu.compiler.TypeAnalyzer.RichType
import annotation.tailrec
import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: xiaomingjia
 * Date: 11/4/11
 * Time: 5:20 PM
 */
object TypeAnalyzer {
  // implicit conversion from Type to RichType
  implicit def type2RichType(t: Type) = new RichType(t)

  // A wrapper that wraps a Type
  class RichType(val typ: Type) {

    def isSameType(t: Type) = typ == t

    def isSubType(t: Type) = {
      if (typ == NullType || isSameType(t))
        true
      else if (!typ.isInstanceOf[RecordType] || !t.isInstanceOf[RecordType])
        false
      else {
        val listA = typ.asInstanceOf[RecordType].fieldTypeList
        val listB = t.asInstanceOf[RecordType].fieldTypeList
        if (listA.length < listB.length) false else isPrefix(listA, listB)
      }
    }

    def isSuperType(t: Type) = t isSubType typ

    def isCastable(t: Type) = {
      t match {
        case p: PrimitiveType => typ.isInstanceOf[PrimitiveType]
        case _ => isSubType(t) || isSuperType(t)
      }
    }

    @tailrec
    private def isPrefix(a: List[FieldType], b: List[FieldType]): Boolean = (a, b) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (x :: xs, y :: ys) => if (x isSameType y) isPrefix(xs, ys) else false
    }
  }

}

class TypeAnalyzer extends ScopeAnalyzer {

  import TypeAnalyzer.type2RichType

  var symbolTable: SymbolTable = _
  var errors : ListBuffer[String] = _

  def error(msg : String) = {
    errors.append(msg)
  }

  def analyze(program: Program) = {
    val scopeAnalyzeResult = analyzeScope(program)
    scopeAnalyzeResult.errors match {
      case list if list.isEmpty => symbolTable = scopeAnalyzeResult.table
      case errors => errors.foreach(println)
    }
  }

  def checkType(node: AstNode) = {

  }

  def checkVarDef(varDef: VarDef) = {
    getType(varDef.expr) match {
      case Right(t) => symbolTable.lookup(varDef.id.name).get.typ = t
      case _ => errors.append("Can't resolve type for varialbe " + varDef.id.name)
    }
  }
  
  def checkAssignStmt(assignStmt : AssignStmt) = {
    val (left, right) = (assignStmt.left, assignStmt.right)
    if (isLeftValue(left)) {
      (getType(left), getType(right)) match {
        case (Right(l), Right(r)) => if (!(r isSubType l)) errors.append("Type mis-match")
        case (l, r) => errors.appendAll(List(l.left, r.left))
      }
    } else {
      errors.append("Left side of the assignemnt must ba an l-value")
    }
  }

  def checkForStmt(forStmt : ForStmt) = {
    getType(forStmt.expr) match {
      case Right(ArrayType(t)) => symbolTable.lookup(forStmt.varId.name).get.typ = t
      case l => error(l.left) error("Expression following for statement must have type of ArrayType")
    }
  }
  
  def checkIfStmt(ifStmt : IfStmt) = {
    getType(ifStmt.expr) match {
      case Right(PrimitiveType("bool")) =>
      case l => error(l.left); error("Expression in if statment must have type of bool")
    }
  }
  
  def checkReturnStmt(returnStmt : ReturnStmt) = {
//    val funDef = symbolTable.currentScope.owner.asInstanceOf[FunDef]
//    val returnType = funDef.typ.toType
//    val actualReturnType = returnStmt.optExpr match {
//      case None => if (returnType != PrimitiveType("void"))
//        error("Type mis-match, can't return value for a function with void return type")
//      case Some(e) => val t = getType(e).isLeft
//    }
//    if (returnType == PrimitiveType("void"))
  }

  def isLeftValue(expr: Expression) = expr match {
    case id: VarId => true
    case s: SubscriptExpr => true
    case f: FieldExpr => true
    case _ => false
  }

  // get type for an expression
  def getType(expr: Expression): Either[String, Type] = expr match {
    case id: VarId => symbolTable.lookup(id.name) match {
      case None => Left("No type info for expr:" + expr.toString)
      case Some(s) => Right(s.typ)
    }
    case bool: BoolLit => Right(PrimitiveType("bool"))
    case int: IntLit => Right(PrimitiveType("int"))
    case string: StringLit => Right(PrimitiveType("string"))
    case n: NullType => Right(NullType())
    case nullLit: NullLit => Right(NullType())
    case array: ArrayLit => getArrayLitType(array)
    case record: RecordLit => getRecordLitType(record)
    case CallExpr(callee, paramList) => getCallExprType(callee, paramList)
    case CastExpr(expr, typ) => getCastExprType(expr, typ)
    case FieldExpr(expr, fieldId) => getFieldExprType(expr, fieldId)
    case InfixExpr(op, left, right) => getInfixExprType(op, left, right)
    case ParenExpr(expr) => getType(expr)
    case PrefixExpr(op, expr) => getPrefixExprType(op, expr)
    case SubscriptExpr(left, right) => getSubscriptExprType(left, right)
  }

  def getSubscriptExprType(left: Expression, right: Expression) = {
    getType(left) match {
      case Right(a: ArrayType) => getType(right) match {
        case Right(PrimitiveType("int")) => Right(a.typ)
        case _ => Left("Subscript expresssion must be type of int")
      }
      case _ => Left("Base exprssion must be type of array")
    }
  }

  def getPrefixExprType(op: String, expr: Expression) = {
    op match {
      case "-" => getType(expr) match {
        case Right(PrimitiveType("int")) => Right(PrimitiveType("int"))
        case _ => Left("Type mis-match, requried int type")
      }
      case "!" => getType(expr) match {
        case Right(PrimitiveType("bool")) => Right(PrimitiveType("bool"))
        case _ => Left("Type mis-match, requried int type")
      }
    }
  }

  def getInfixExprType(op: String, left: Expression, right: Expression): Either[String, Type] = {
    op match {
      case "||" | "&&" => (getType(left), getType(right)) match {
        case (l: Left[_, _], _) => l
        case (_, r: Left[_, _]) => r
        case (Right(PrimitiveType("bool")), Right(PrimitiveType("bool"))) => Right(PrimitiveType("bool"))
        case _ => Left("Type mismatch, \"" + op + "\" can only operate with bool type")
      }
      case "==" | "!=" => (getType(left), getType(right)) match {
        case (Right(NullType()), _) | (_, Right(NullType())) => Right(PrimitiveType("bool"))
        case (Right(l), Right(r)) => if ((l isSameType r) || (l isCastable r)) Right(PrimitiveType("bool")) else Left("Type mismatch")
        case _ => Left("Type mismatch, \"" + op + "\" can only operate with bool type")
      }
      case "<=" | "<" | ">=" | ">" => (getType(left), getType(right)) match {
        case (Right(PrimitiveType("int")), Right(PrimitiveType("int"))) => Right(PrimitiveType("bool"))
        case _ => Left("Type mismatch, \"" + op + "\" can only operate with int type")
      }
      case "+" => (getType(left), getType(right)) match {
        case (Right(PrimitiveType("string")), _) | (_, Right(PrimitiveType("string"))) => Right(PrimitiveType("string"))
        case (Right(PrimitiveType("int")), Right(PrimitiveType("int"))) => Right(PrimitiveType("int"))
        case _ => Left("Type mismatch, \"" + op + "\" can only operate with int type and string type")
      }
      case "-" | "*" | "/" | "%" => (getType(left), getType(right)) match {
        case (Right(PrimitiveType("int")), Right(PrimitiveType("int"))) => Right(PrimitiveType("int"))
        case _ => Left("Type mismatch, \"" + op + "\" can only operate with int type and string type")
      }
    }
  }

  def getFieldExprType(expr: Expression, fieldId: FieldId) = {
    getType(expr) match {
      case r: RecordType => {
        r.fieldTypeList.view.filter(_.fieldId == fieldId) match {
          case l if l.isEmpty => Left("expression doesn't have filed \"" + fieldId.name + "\"")
          case l => Right(l.head.typ)
        }
      }
    }
  }

  def getCastExprType(expr: Expression, typ: Type) = {
    getType(expr) match {
      case Right(t) => if (t isCastable typ) Right(typ) else Left("type cast error")
      case l => l
    }
  }

  def getCallExprType(callee: FunId, paramList: List[Expression]) = {
    val funDef = symbolTable.lookup(callee.name)
    funDef match {
      case None => Left("Funciton " + callee.name + " is not defined")
      case Some(s) => {
        val (from, to) = s.typ.asInstanceOf[FunType] match {
          case FunType(f: RecordType, t: Type) => (f, t)
        }
        val paramTypeList = paramList.map(getType)
        val valid = !paramTypeList.exists(_.isLeft)
        if (valid && from.fieldTypeList.map(_.typ) == paramTypeList.map(_.right)) {
          Right(to)
        } else {
          Left("type mis-match for function \"" + callee.name + "\"")
        }
      }
    }
  }

  def getArrayLitType(array: ArrayLit): Either[String, Type] = {
    val types = array.exprList.map((e) => getType(e))
    if (types.isEmpty || types.head.isLeft)
      Left("Can't resolve type for varialbe " + array.toString)
    else {
      val t = types.head
      types.dropWhile(_ == t) match {
        case Nil => t
        case _ => Left("Different types in array " + array.toString)
      }
    }
  }

  def getRecordLitType(record: RecordLit): Either[String, Type] = {
    @tailrec
    def addToRecordType(fields: List[FieldLit], types: List[FieldType]): Either[String, RecordType] = {
      fields match {
        case Nil => Right(RecordType(types.reverse))
        case x :: xs => {
          val name = x.fieldId.name
          val typ = getType(x.expr)
          if (typ.isLeft)
            Left("Can't resove type for filed " + name + " in " + x.toString)
          else
            addToRecordType(xs, FieldType(FieldId(name), typ.right.get) :: types)
        }
      }
    }
    addToRecordType(record.fieldLitList, Nil)
  }
}