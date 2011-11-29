package edu.nyu.compiler

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
      if (typ.isInstanceOf[NullType] || isSameType(t))
        true
      else if (typ.isInstanceOf[NullType] && t.isInstanceOf[RecordType])
        true
      else if (typ.isInstanceOf[RecordType] && t.isInstanceOf[RecordType]) {
        val listA = typ.asInstanceOf[RecordType].fieldTypeList
        val listB = t.asInstanceOf[RecordType].fieldTypeList
        if (listA.length < listB.length) false else isPrefix(listA, listB)
      } else false
    }

    def isSuperType(t: Type) = t isSubType typ

    def canCastTo(t: Type) = {
      typ match {
        case p: PrimitiveType => t.isInstanceOf[PrimitiveType]
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

trait TypeAnalyzer extends ScopeAnalyzer {

  import TypeAnalyzer.type2RichType

  val int = PrimitiveType("int")
  val string = PrimitiveType("string")
  val bool = PrimitiveType("bool")

  var symbolTable: SymbolTable = _
  var errors = new ListBuffer[String]

  def error(msg: String) {
    errors.append(msg)
  }

  def analyzeType(program: Program) : Either[String, String] = {
    val scopeAnalyzeResult = analyzeScope(program)
    scopeAnalyzeResult.errors match {
      case list if list.isEmpty => {
        symbolTable = scopeAnalyzeResult.table
        checkType(program)
        if (!errors.isEmpty) {
          Left(errors.mkString("\n") + "\nThere are " + errors.size + " errors.")
        }
        else Right(symbolTable.topLevel.getStringRep(0))
      }
      case e => Left(e.mkString("\n") + "\nThere are " + e.size + " errors.")
    }
  }

  def checkType(node: AstNode) {
    node match {
      case p: Program => checkProgram(p)
      case f: FunDef => checkFunDef(f)
      case v: VarDef => checkVarDef(v)
      case a: AssignStmt => checkAssignStmt(a)
      case c: CallStmt => checkCallStmt(c)
      case b: BlockStmt => checkBlockStmt(b)
      case f: ForStmt => checkForStmt(f)
      case i: IfStmt => checkIfStmt(i)
      case r: ReturnStmt => checkReturnStmt(r)
      case w: WhileStmt => checkWhileStmt(w)
      case _ =>
    }
  }

  def checkProgram(program: Program) {
    program.funList.foreach(checkType)
  }

  def checkFunDef(funDef: FunDef) {
    symbolTable.push(funDef.scope, false)
    funDef.blockStmt.stmtList.foreach(checkType)
    symbolTable.pop()
  }

  def checkBlockStmt(blockStmt: BlockStmt) {
    symbolTable.push(blockStmt.scope, false)
    blockStmt.stmtList.foreach(checkType)
    symbolTable.pop()
  }

  def checkVarDef(varDef: VarDef) {
    getType(varDef.expr) match {
      case Right(t) => symbolTable.lookup(varDef.varId.name).get.typ = t
      case Left(e) => ("Could not resolve type for varialbe '" + varDef.varId.name + "'" :: e).reverse.foreach(error)
    }
  }

  def checkAssignStmt(assignStmt: AssignStmt) {
    val (left, right) = (assignStmt.left, assignStmt.right)
    if (isLeftValue(left)) {
      val name = getVariableName(left)
      symbolTable.lookup(name) match {
        case Some(s) if s.definition == null || s.definition.isInstanceOf[FunDef] => error("Variable name expected, found : '" + name + "'")
        case Some(s) =>
          (getType(left), getType(right)) match {
            case (Right(l), Right(r)) => if (!(r isSubType l)) error("Cannot  assign " + l.toString + " from " + r.toString)
            case (l@Left(e), _) => e.foreach(error)
            case (_, l@Left(e)) => e.foreach(error)
          }
        case None => error("Unknow variable '" + name + "'")
      }
    } else {
      error("Variable name expected")
    }
  }

  def getVariableName(node: AstNode): String = node match {
    case v: VarId => v.name
    case s: SubscriptExpr => getVariableName(s.left)
    case f: FieldExpr => getVariableName(f.expr)
    case _ => ""
  }


  def checkCallStmt(callStmt: CallStmt) {
    getType(callStmt.expr) match {
      case Right(t) =>
      case Left(e) => e.foreach(error)
    }
  }

  def checkForStmt(forStmt: ForStmt) {
    symbolTable.push(forStmt.scope, false)
    getType(forStmt.expr) match {
      case Right(ArrayType(t)) => symbolTable.lookup(forStmt.varId.name).get.typ = t
      case Right(t) => error("Subject of for-loop must be array")
      case l => error("Could not resolve type for variable '" + forStmt.varId.name + "'")
    }
    symbolTable.pop()
  }

  def checkIfStmt(ifStmt: IfStmt) {
    getType(ifStmt.expr) match {
      case Right(PrimitiveType("bool")) =>
      case Right(t) => error("Boolean expected")
      case Left(e) => e.foreach(error)
    }
    checkBlockStmt(ifStmt.blockStmt)
    ifStmt.optBlockStmt match {
      case Some(s) => checkBlockStmt(s)
      case None =>
    }
  }

  def checkReturnStmt(returnStmt: ReturnStmt) {
    val funDef = symbolTable.lookupFunDef(symbolTable.currentScope) match {
      case Some(f) => f
      case _ => throw new IllegalStateException("Unexpected return statement")
    }
    val returnType = funDef.typ.returnType
    returnStmt.optExpr match {
      case None => {
        if (returnType != PrimitiveType("void"))
          error("Expected return value of type'" + returnType.toString + "', found 'void'")
      }
      case Some(expr) => getType(expr) match {
        case Right(t) => {
          if (!(t isSubType returnType))
            error(typeMismatch(returnType, t))
        }
        case Left(e) => e.foreach(error)
      }
    }
  }

  def checkWhileStmt(whileStmt: WhileStmt) {
    getType(whileStmt.expr) match {
      case Right(PrimitiveType("bool")) =>
      case Right(t) => error("Boolean expected")
      case Left(e) => e.foreach(error)
    }
    checkBlockStmt(whileStmt.blockStmt)
  }

  def typeMismatch(expected: Type, found: Type) = {
    "Expected return type of '" + expected.toString + "', found '" + found.toString + "'"
  }

  def isLeftValue(expr: Expression) = expr match {
    case v: VarId => true
    case f: FieldExpr => true
    case s: SubscriptExpr => true
    case _ => false
  }

  // get type for an expression
  def getType(expression: Expression): Either[List[String], Type] = expression match {
    case id: VarId => symbolTable.lookup(id.name) match {
      case None => Left(List("Unknown variable '" + id.name + "'"))
      case Some(s) if s.typ != null => Right(s.typ)
      case _ => Left(List("Could not resolve type"))
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
        case Right(PrimitiveType("int")) => Right(a.elementType)
        case Right(t) => Left(List("Integer expected"))
        case l => l
      }
      case Right(typ) => getType(right) match {
        case Right(PrimitiveType("int")) => Left(List("Base of subscript must be array"))
        case Right(t) => Left(List("Base of subscript must be array", "Integer expected"))
        case l => l
      }
      case l@Left(e) => l
    }
  }

  def getPrefixExprType(op: String, expr: Expression) = {
    op match {
      case "-" => getType(expr) match {
        case Right(PrimitiveType("int")) => Right(PrimitiveType("int"))
        case Right(t) => Left(List("Integer expected"))
        case l@Left(e) => Left("Could not resolve type for " + expr.toString :: e)
      }
      case "!" => getType(expr) match {
        case Right(PrimitiveType("bool")) => Right(PrimitiveType("bool"))
        case Right(t) => Left(List("Boolean expected"))
        case l@Left(e) => Left("Could not resolve type for " + expr.toString :: e)
      }
    }
  }

  def getInfixExprType(op: String, left: Expression, right: Expression) = {
    op match {
      case "||" | "&&" => (getType(left), getType(right)) match {
        case (Right(PrimitiveType("bool")), Right(PrimitiveType("bool"))) => Right(PrimitiveType("bool"))
        case (l@Left(e), _) => l
        case (_, r@Left(e)) => r
        case _ => Left(List(("Boolean expected")))

      }
      case "==" | "!=" => (getType(left), getType(right)) match {
        case (Right(NullType()), _) | (_, Right(NullType())) => Right(PrimitiveType("bool"))
        case (Right(l), Right(r)) => {
          if ((l isSameType r) || (l canCastTo r))
            Right(PrimitiveType("bool"))
          else Left(List("Can't compare '" + l.toString + "' to '" + r.toString + "'"))
        }
        case (l@Left(e), _) => l
        case (_, r@Left(e)) => r
      }
      case "<=" | "<" | ">=" | ">" => (getType(left), getType(right)) match {
        case (Right(PrimitiveType("int")), Right(PrimitiveType("int"))) => Right(PrimitiveType("bool"))
        case (l@Left(e), _) => l
        case (_, r@Left(e)) => r
        case _ => Left(List("Integer expected"))
      }
      case "+" => (getType(left), getType(right)) match {
        case (Right(PrimitiveType("string")), _) | (_, Right(PrimitiveType("string"))) => Right(PrimitiveType("string"))
        case (Right(PrimitiveType("int")), Right(PrimitiveType("int"))) => Right(PrimitiveType("int"))
        case (l@Left(e), _) => l
        case (_, r@Left(e)) => r
        case _ => Left(List("Integer expected"))
      }
      case "-" | "*" | "/" | "%" => (getType(left), getType(right)) match {
        case (Right(PrimitiveType("int")), Right(PrimitiveType("int"))) => Right(PrimitiveType("int"))
        case (l@Left(e), _) => l
        case (_, r@Left(e)) => r
        case _ => Left(List("Integer expected"))
      }
    }
  }

  def getFieldExprType(expr: Expression, fieldId: FieldId) = {
    getType(expr) match {
      case Right(r: RecordType) => r.fieldTypeList.view.filter(_.fieldId == fieldId) match {
        case l if l.isEmpty => Left(List("Unknown filed '" + fieldId.name + "'"))
        case l => Right(l.head.typ)
      }
      case Right(t) => Left(List("Left side of expression must be record type, found " + t.toString))
      case l => l

    }
  }

  def getCastExprType(expr: Expression, typ: Type) = {
    getType(expr) match {
      case Right(t) => {
        if (t canCastTo typ)
          Right(typ)
        else
          Left(List("Can't cast from type " + t.toString + " to type " + typ.toString))
      }
      case l => l
    }
  }

  def getCallExprType(callee: FunId, paramList: List[Expression]) = {
    val funDefSymbol = symbolTable.lookup(callee.name)
    funDefSymbol match {
      case None => Left(List("Unknown function '" + callee.name + "'"))
      case Some(s) if s.definition == null || s.definition.isInstanceOf[FunDef] => {
        val funType = s.typ.asInstanceOf[FunType]
        val (from, to) = (funType.fromType, funType.returnType)
        val paramTypeList = paramList.map(getType)
        val valid = !paramTypeList.exists(_.isLeft)
        if (valid && from.fieldTypeList.size == paramTypeList.size) {
          val actualParamTypes = paramTypeList.map(_.right.get)
          callee.name match {
            case "size" => {
              if (actualParamTypes.size == 1 && actualParamTypes.head.isInstanceOf[ArrayType])
                Right(int)
              else Left(List("Formal 'a' of function size expectes type array, found " + actualParamTypes.mkString("(", ",", ")") + " instead"))
            }
            case _ => {
              // check if actual parameters' type conforms function formal parameters' type
              val list = from.fieldTypeList.zip(actualParamTypes)
              val e = for ((expected, found) <- list if !found.isSameType(expected.typ) && !found.isSubType(expected.typ)) yield "Formal '" +
                expected.fieldId.name + "' of function '" + callee.name + "' expectes '" + expected.typ.toString + "' found '" + found.toString + "' instead"

              if (e.isEmpty) Right(to)
              else Left(e.reverse)
            }
          }
        } else if (valid) {
          Left(List("Wrong number of arguments for funciton '" + callee.name))
        } else {
          Left(paramTypeList.filter(_.isLeft).map(_.left.get).flatten)
        }
      }
      case Some(s) => Left(List("Function name exptected, found : " + callee.name))
    }
  }

  def getArrayLitType(array: ArrayLit) = {
    val types = array.exprList.map((e) => getType(e))
    if (types.isEmpty || types.exists(_.isLeft))
      Left(List("Could not resolve array element type"))
    else {
      val t = types.head
      types.dropWhile(_ == t) match {
        case Nil => 
          array.typ = ArrayType(t.right.get)
          Right(array.typ)
        case _ => Left(List("Different types in array " + types.map(_.right.get.toString).mkString("[", ",", "]")))
      }
    }
  }

  def getRecordLitType(record: RecordLit) = {
    @tailrec
    def addToRecordType(fields: List[FieldLit], types: List[FieldType]): Either[List[String], RecordType] = {
      fields match {
        case Nil =>
          record.typ = RecordType(types.reverse)
          Right(record.typ)
        case x :: xs => {
          val name = x.fieldId.name
          val typ = getType(x.expr)
          if (typ.isLeft)
            Left(List("Could not resolve type for field '" + name + "'"))
          else
            addToRecordType(xs, FieldType(FieldId(name), typ.right.get) :: types)
        }
      }
    }
    addToRecordType(record.fieldLitList, Nil)
  }
}