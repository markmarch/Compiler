package edu.nyu.compiler

sealed abstract class Instruction

// top level
case class ProgramIr(funList : List[FunIr]) {
  override def toString = funList.map(_.toString).mkString("\n")
}

case class FunIr(id : String, funType: FunType, stmtList: List[Instruction]) {
  override def toString = id + " = fun " + funType.toString +"\n" + stmtList.map(_.toString + ";").mkString("\n")
}

// Address
sealed  abstract class Address

case class NameAddr(id : String) extends Address{
  override def toString = id
}

case class ConstAddr(lit : String) extends Address{
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

case class InfixInstr(address : Address, op: String, left:Address, right:Address) extends Instruction{
  override def toString = address.toString + " = " + left.toString + " " + op + " " + right.toString
}

case class PrefixInstr(address : Address,  op: String, operand: Address) extends Instruction {
  override def toString = address.toString + " = " + op + operand.toString
}

case class CastInstr(address:Address, from: Address, typ: Type) extends Instruction {
  override def toString = address.toString + " = " + from.toString + ":" + typ.toString
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

case class CallInstr(oa : Option[Address], funId : String,  arity: Int) extends Instruction {
  override def toString = oa.map(_.toString + " = ").getOrElse("") + "call " + funId + " : " + arity
}

case class ReturnInstr(oa : Option[Address]) extends Instruction {
  override def toString = "return" + oa.map(" " + _.toString).getOrElse("")
}

case class ArrayReadInstr(address: Address, array: Address, index: Address) extends Instruction {
  override def toString = address.toString + " = " + array.toString + "[" + index.toString + "]"
}

case class ArrayWriteInstr(array:Address, index: Address,  address: Address) extends Instruction {
  override def toString = array.toString + "[" + index.toString + "] = " + address.toString
}

case class RecReadInstr(address: Address, record: Address, fieldId: String) extends Instruction {
  override def toString = address.toString + " = " + record.toString + "." + fieldId
}

case class RecWriteInstr(record: Address, fieldId: String, address: Address) extends Instruction {
  override def toString = record.toString + "." + fieldId + " = " + address.toString
}