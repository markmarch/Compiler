package edu.nyu.compiler

import collection.mutable.{HashMap, ListBuffer}
import annotation.tailrec


/**
 * Created by IntelliJ IDEA.
 * User: xmj
 * Date: 12/11/11
 * Time: 5:33 PM
 */

trait CodeGenerator {

  import x64._

  var sp = 0
  val offsetMap = HashMap.empty[String, Int]
  val roData = HashMap.empty[String, HashMap[String, String]]
  var currentFun: FunIr = _

  val argsRegister = List("rdi", "rsi", "rdx", "rcx", "r8", "r9")
  val infixOpMap = Map("+" -> "add", "-" -> "sub", "*" -> "imul")
  val relOpMap = Map(">=" -> "jge", ">" -> "jg", "<=" -> "jle", "<" -> "jl", "==" -> "je", "!=" -> "jne")
  val relReverseMap = Map(">=" -> "<", "<" -> ">=", "<=" -> ">", ">" -> "<=")

  val sizeMap = HashMap.empty[Type, Int]

  val fieldOffsetMap = HashMap.empty[(RecordType, String), Int]

  val ret = List(mov("rsp", "rbp"), "pop  rbp", "ret")

  def generate(p: ProgramIr): String = {
    "\t.intel_syntax noprefix\n" + p.funList.map(generateFun).mkString("\n")
  }

  def generateFun(f: FunIr): String = {
    sp = f.frameSize
    currentFun = f
    offsetMap.clear()
    val header = ".text\n\t" + ".global " + f.id + "\n\t.type\t" + f.id + ", @function\n " + f.id + ":\n"
    val saveStack = List("push rbp", mov("rbp", "rsp"), "sub  rsp, " + sp)
    // get function arguments
    val argAddr = f.funType.fromType.fieldTypeList.map(_.fieldId.symbol.address.asInstanceOf[NameAddr])
    val getArgs = argsRegister.zip(argAddr).map(p => mov(p._2, p._1)) :::
      argAddr.drop(argsRegister.size).map("pop " + getAddress(_))
    val body = saveStack ::: getArgs ::: f.stmtList.flatMap(genInstr)
    val rest = " %s.end:\n\t.size\t%s, .-%s".format(f.id, f.id, f.id)
    header + body.mkString("\t", "\n\t", "\n") + rest + getRodata(f.id)
  }

  def getRodata(funId: String): String = roData.get(funId) match {
    case Some(m) =>
      val data = for (lit <- m.keys) yield " " + m.get(lit).get + ":\n\t.string " + lit
      "\n.section .rodata" + data.mkString("\n", "\n", "\n")
    case None => ""
  }

  def genInstr(instr: Instruction): List[String] = {
    instr match {
      case l: EmptyLabel => List(l.label + ":")
      case c: CallInstr => genCallInstr(c)
      case r: ReturnInstr => genReturnInstr(r)
      case p: PrefixInstr => genPrefixInstr(p)
      case i: InfixInstr => genInfixInstr(i)
      case c: CopyInstr => genCopyInstr(c)
      case r: RelopJumpInstr => genRelopJumInstr(r)
      case l: LabeledInstruction => l.label + ":" :: genInstr(l.instr)
      case a: ArrayWriteInstr => genArrayWriteInstr(a)
      case a: ArrayReadInstr => genArrayReadInstr(a)
      case r: RecWriteInstr => genRecdWriteInstr(r)
      case r: RecReadInstr => genRecReadInstr(r)
      case u: UnconditionalJumpInstr => List("jmp  " + u.label)
      case t: TrueJumpInstr => genTrueJumpInstr(t)
      case f: FalseJumpInstr => genFalseJumpInstr(f)
      case c: CastInstr => genCastInstr(c)
    }
  }

  def genCastInstr(c: CastInstr): List[String] = {
    c.address.typ match {
      case to: PrimitiveType if c.typ != to =>
        val castFun = c.typ.toString + "2" + to.toString
        val paramList = List(ParamInstr(0, 1, c.from))
        genCallInstr(CallInstr(paramList, Some(c.address), castFun, 1))
      case _ => genCopyInstr(CopyInstr(c.address, c.from))
    }
  }

  def genArrayWriteInstr(a: ArrayWriteInstr): List[String] = {
    val t = a.array.typ.asInstanceOf[ArrayType]
    a.index match {
      case na: NameAddr =>
        List(
          mov("rax", "QWORD PTR [rbp-" + getOffset(a.array) + "]"),
          mov("rbx", "QWORD PTR [rax+8]"),
          mov("rdx", na),
          mov("rax", a.address),
          mov("QWORD PTR [rbx+rdx*8]", "rax"))
      case ConstAddr(index, `int`) =>
        val offset = index.toInt * 8
        List(
          mov("rax", "QWORD PTR [rbp-" + getOffset(a.array) + "]"),
          mov("rbx", "QWORD PTR [rax+8]"),
          mov("rax", a.address),
          mov("QWORD PTR [rbx+" + offset + "]", "rax"))
    }
  }

  def genArrayReadInstr(a: ArrayReadInstr): List[String] = {
    val t = a.array.typ.asInstanceOf[ArrayType]
    a.index match {
      case na: NameAddr =>
        List(
          mov("rax", "QWORD PTR [rbp-" + getOffset(a.array) + "]"),
          mov("rbx", "QWORD PTR [rax+8]"),
          mov("rdx", na),
          mov("rax", "QWORD PTR [rbx+rdx*8]"),
          mov(a.address, "rax"))
      case ConstAddr(index, `int`) =>
        val offset = index.toInt * 8
        List(
          mov("rax", "QWORD PTR [rbp-" + getOffset(a.array) + "]"),
          mov("rbx", "QWORD PTR [rax+8]"),
          mov("rax", "QWORD PTR [rbx+" + offset + "]"),
          mov(a.address, "rax"))
    }
  }

  def genRecdWriteInstr(r: RecWriteInstr): List[String] = {
    val t = r.record.typ.asInstanceOf[RecordType]
    val offset = getFieldOffset(t, r.fieldId)
    if (offset == 0) {
      List(mov("rax", r.address), mov("rbx", "QWORD PTR[rbp-" + getOffset(r.record) + "]"), mov("QWORD PTR [rbx]", "rax"))
    } else {
      List(mov("rax", r.address),
        mov("rbx", "QWORD PTR [rbp-" + getOffset(r.record) + "]"),
        mov("QWORD PTR [rbx+" + offset + "]", "rax"))
    }

  }

  def genRecReadInstr(r: RecReadInstr): List[String] = {
    val t = r.record.typ.asInstanceOf[RecordType]
    val offset = getFieldOffset(t, r.fieldId)
    if (offset == 0) {
      List(
        mov("rbx", "QWORD PTR [rbp-" + getOffset(r.record) + "]"),
        mov("rax", "QWORD PTR[rbx]"),
        mov(r.address, "rax"))
    } else {
      List(
        mov("rbx", "QWORD PTR [rbp-" + getOffset(r.record) + "]"),
        mov("rax", "QWORD PTR [rbx+" + offset + "]"), mov(r.address, "rax"))
    }

  }

  def genPrefixInstr(p: PrefixInstr): List[String] = p.op match {
    case "-" => List(mov("rax", getAddress(p.operand)), "neg  rax", mov(getAddress(p.address), "rax"))
  }

  def genTrueJumpInstr(t: TrueJumpInstr): List[String] = {
    List("cmp " + getAddress(t.address) + ", 1", "je " + t.label)
  }

  def genFalseJumpInstr(f: FalseJumpInstr): List[String] = {
    List("cmp " + getAddress(f.address) + ", 0", "je " + f.label)
  }

  def genRelopJumInstr(r: RelopJumpInstr): List[String] = {
    (r.left, r.right) match {
      case (ac: ConstAddr, bc: ConstAddr) =>
        val a :: b :: _ = List(ac, bc).map(getAddress(_).toInt)
        val result = r.op match {
          case ">=" => a >= b
          case "<" => a < b
          case "<=" => a <= b
          case ">" => a > b
          case "==" => a == b
          case "!=" => a != b
        }
        genTrueJumpInstr(TrueJumpInstr(ConstAddr(result.toString, bool), r.label))
      case (ca: ConstAddr, _: NameAddr) => genRelopJumInstr(RelopJumpInstr(relReverseMap.get(r.op).get, r.right, r.left, r.label))
      case _ =>
        r.right match {
          case na: NameAddr => List(mov("rax", na), "cmp  " + getAddress(r.left) + ", rax", relOpMap.get(r.op).get + " " + r.label)
          case _ => List("cmp  " + getAddress(r.left) + ", " + getAddress(r.right), relOpMap.get(r.op).get + " " + r.label)
        }
    }


  }

  def genInfixInstr(i: InfixInstr): List[String] = i.op match {
    case "+" | "-" => genAddSubInstr(i.op, i.address, i.left, i.right)
    case "/" | "%" =>
      val last = i.op match {
        case "%" => mov(getAddress(i.address), "rdx")
        case "/" => mov(getAddress(i.address), "rax")
      }

      val div = i.right match {
        case _: NameAddr => List("idiv " + getAddress(i.right))
        case _ => List(mov("rcx", getAddress(i.right)), "idiv rcx")
      }
      mov("rax", getAddress(i.left)) :: mov("rdx", "rax") :: div ::: List(last)
    case "*" => i.right match {
      case _: NameAddr => List(mov("rax", getAddress(i.left)), "imul rax" + ", " + getAddress(i.right), mov(getAddress(i.address), "rax"))
      case _ => List(mov("rax", getAddress(i.left)), mov("rdx", getAddress(i.right)), "imul rax, rdx", mov(getAddress(i.address), "rax"))
    }
  }


  def genAddSubInstr(op: String, a: NameAddr, left: Address, right: Address): List[String] = {
    (left, right) match {
      case (_: NameAddr, _) | (_, _: NameAddr) =>
        List(mov("rax", getAddress(left)), addSub(op, "rax", getAddress(right)), mov(getAddress(a), "rax"))
      case _ => List(mov(getAddress(a), getAddress(left)), addSub(op, getAddress(a), getAddress(right)))
    }
  }

  def genReturnInstr(r: ReturnInstr): List[String] = r.oa match {
    case Some(addr) => List(mov("rax", getAddress(addr))) ::: ret
    case None => ret
  }


  def genCopyInstr(c: CopyInstr): List[String] = {
    println(c.left + " = " + c.right)
    c.right match {
      case na: NameAddr => List(mov("rax", getAddress(c.right)), mov(getAddress(c.left), "rax"))
      case _ => List(mov(getAddress(c.left), getAddress(c.right)))
    }
  }

  def genCallInstr(ci: CallInstr): List[String] = {
    val callInstr = prepareArgs(ci.paramList).reverse ::: List(mov("rax", "0"), call(ci.funId))
    ci.oa match {
      case Some(a) =>
        val na = a.asInstanceOf[NameAddr]
        callInstr ::: List(mov(a, "rax"))
      case None => callInstr
    }
  }

  def prepareArgs(args: List[ParamInstr]): List[String] = {
    args.zip(argsRegister).map(p => mov(p._2, p._1.address)) :::
      args.drop(argsRegister.size).map(a => "push " + getAddress(a.address))
  }

  def getOffset(addr: NameAddr): Int = offsetMap.get(addr.name) match {
    case Some(offset) => offset
    case None =>
      offsetMap.put(addr.name, sp)
      sp -= 8
      sp + 8
  }


  def getFieldOffset(r: RecordType, f: String): Int = {
    @tailrec
    def calculate(l: List[FieldType], offset: Int): Int = l match {
      case x :: xs if x.fieldId.name == f => offset
      case x :: xs => calculate(xs, offset + 8)
    }

    fieldOffsetMap.get((r, f)) match {
      case Some(offset) => offset
      case None =>
        val offset = calculate(r.fieldTypeList, 0)
        fieldOffsetMap.put((r, f), offset)
        offset
    }
  }

  def getStringLitLabel(funId: String, lit: String): String = {
    val m = roData.getOrElseUpdate(funId, HashMap.empty[String, String])
    m.getOrElseUpdate(lit, funId + ".S_" + m.size)
  }

  def getTypeSize(typ: Type): Int = {
    def size(typ: Type): Int = typ match {
      case PrimitiveType(_) => 8
      case r: RecordType => r.fieldTypeList.size * 8
      case a: ArrayType => 8
      case f: FieldType => getTypeSize(f.typ)
    }
    sizeMap.getOrElseUpdate(typ, size(typ))
  }

  object x64 {

    def getAddress(a: Address) = a match {
      case na: NameAddr => "QWORD PTR [rbp-" + getOffset(na) + "]"
      case ConstAddr(_ , _: NullType) => "0"
      case ConstAddr(lit, `int`) => lit
      case ConstAddr(lit, `bool`) => if (lit == "true") "1" else "0"
      case ConstAddr(lit, `string`) => "OFFSET FLAT:" + getStringLitLabel(currentFun.id, lit)
      case SizeOfAddr(typ) => getTypeSize(typ).toString
    }

    def mov(to: String, from: String): String = "mov  " + to + ", " + from

    def mov(to: String, from: Address): String = mov(to, getAddress(from))

    def mov(to: Address, from: String): String = mov(getAddress(to), from)

    def addSub(op: String, a: String, b: String) = {
      val instr = op match {
        case "+" => "add  "
        case "-" => "sub  "
        case "*" => "imul "
      }
      instr + a + ", " + b
    }

    def call(funName: String) = "call " + funName

    def infixOp(op: String, des: String, operand: String) = infixOpMap.get(op).get + "  " + des + ", " + operand
  }

}