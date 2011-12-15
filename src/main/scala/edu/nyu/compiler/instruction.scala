package edu.nyu.compiler

sealed abstract class Instruction

// top level
case class ProgramIr(funList : List[FunIr]) {
  def getStringRep : List[String] = funList.flatMap(
    f => f.id + " = fun " + f.funType.toString :: f.stmtList.map(instr => instr match {
      case e: EmptyLabel => e.toString
      case i => "  " + i.toString + ";"
    }))
  override def toString = funList.map(_.toString).mkString("\n")
}

case class FunIr(id : String, funType: FunType, stmtList: List[Instruction], frameSize: Int) {
  override def toString = id + " = fun " + funType.toString +"\n" + stmtList.map(_.toString + ";").mkString("\n")
}

// Address
sealed  abstract class Address

case class NameAddr(name : String, typ : Type) extends Address{
  override def toString = name
}

case class ConstAddr(lit : String, typ : Type) extends Address{
  override def toString = lit
}

case class SizeOfAddr(typ : Type) extends Address {
  override def toString = "sizeof(" + typ.toString + ")"
}

// Instructions
case class LabeledInstruction(label : String, instr : Instruction) extends Instruction {
  override def toString = label + ": " + instr.toString
}

case class EmptyLabel(label : String) extends Instruction {
  override def toString = label + ":"
}

case class CopyInstr(left: Address, right: Address) extends Instruction {
  override def toString = left.toString + " = " + right.toString
}

case class InfixInstr(address : NameAddr, op: String, left:Address, right:Address) extends Instruction{
  override def toString = address.toString + " = " + left.toString + " " + op + " " + right.toString
}

case class PrefixInstr(address : NameAddr,  op: String, operand: Address) extends Instruction {
  override def toString = address.toString + " = " + op + operand.toString
}

case class CastInstr(address:NameAddr, from: Address, typ: Type) extends Instruction {
  override def toString = address.toString + " = " + from.toString + ":" + address.typ.toString
}

case class UnconditionalJumpInstr(label : String) extends Instruction {
  override def toString = "goto "+ label
}

case class TrueJumpInstr(address : Address, label : String) extends Instruction {
  override def toString = "if " + address.toString + " goto " + label
}

case class FalseJumpInstr(address : Address, label: String) extends Instruction {
  override def toString = "ifFalse " + address.toString + " goto " + label
}

case class RelopJumpInstr(op : String, left : Address, right: Address, label: String) extends Instruction {
  override def toString = "if " + left.toString + " " + op + " " + right.toString + " goto " + label
}

case class ParamInstr(index: Int,  arity: Int,  address: Address) extends Instruction {
  override def toString = "param [" + index + ":" + arity + "] = " + address.toString  
}

case class CallInstr(paramList : List[ParamInstr], oa : Option[Address], funId : String,  arity: Int) extends Instruction {
  override def toString = paramList.map(_.toString + ";").mkString("", "\n  ", "\n  ") +
    oa.map(_.toString + " = ").getOrElse("") + "call " + funId + " : " + arity
}

case class ReturnInstr(oa : Option[Address]) extends Instruction {
  override def toString = "return" + oa.map(" " + _.toString).getOrElse("")
}

case class ArrayReadInstr(address: NameAddr, array: NameAddr, index: Address) extends Instruction {
  override def toString = address.toString + " = " + array.toString + "[" + index.toString + "]"
}

case class ArrayWriteInstr(array:NameAddr, index: Address,  address: Address) extends Instruction {
  override def toString = array.toString + "[" + index.toString + "] = " + address.toString
}

case class RecReadInstr(address: NameAddr, record: NameAddr, fieldId: String) extends Instruction {
  override def toString = address.toString + " = " + record.toString + "." + fieldId
}

case class RecWriteInstr(record: NameAddr, fieldId: String, address: Address) extends Instruction {
  override def toString = record.toString + "." + fieldId + " = " + address.toString
}