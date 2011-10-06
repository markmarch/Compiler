package edu.nyu.compiler

import scala.util.parsing.combinator.{PackratParsers, ImplicitConversions}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

trait TackParser extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  type PAny = PackratParser[Any]
  // replace the default lexical with TackLexical
  override val lexical = new TackLexical

  lexical.delimiters ++= TackParser.delimiters

  lexical.reserved ++= TackParser.reserved

  def program = (funDef +)

  def funDef = ident ~ "=" ~ "fun" ~ funType ~ blockStmt

  def _type: Parser[Any] = arrayType | recordType | "bool" | "int" | "string"

  def arrayType = "[" ~ _type ~ "]"

  def recordType = "(" ~ repsep(fieldType, ",") ~ ")"

  def fieldType = ident ~ ":" ~ _type

  def funType = recordType ~ "->" ~ returnType

  def returnType = _type | "void"

  def stmt: Parser[Any] = varDef | assignStmt | blockStmt | callStmt | forStmt | ifStmt | returnStmt | whileStmt

  def varDef = ident ~ "=" ~ expr ~ ";"

  def assignStmt = expr ~ (":=" ~ expr) ~ ";"

  def blockStmt = "{" ~> rep(stmt) <~ "}"

  def callStmt = Expressions.callExpr ~ ";" ^^ {
    case a => println("call stmt"); a
  }

  def forStmt = "for" ~ ident ~ "in" ~ expr ~ blockStmt

  def ifStmt = "if" ~ expr ~ blockStmt ~ opt("else" ~ blockStmt)

  def returnStmt = "->" ~ opt(expr) ~ ";"

  def whileStmt = "while" ~ expr ~ blockStmt

  object Expressions {
    // () and literals
    lazy val expr1: PackratParser[Any] = (ident | literal | "(" ~> expr9 <~ ")") ^^ {
      case a => println("expr1 " + a.toString); a
    }

    // postfix call, cast , field and subscript
    lazy val callExpr = expr1 ~ ("(" ~> repsep(expr9, ",") <~ ")") ^^ {
      case a => println("call expr"); a
    }
    lazy val castExpr = expr2 ~ (":" ~> _type) ^^ {
      case a => println("cast"); a
    }
    lazy val fieldExpr = expr2 ~ "." ~> ident
    lazy val subscriptExpr = expr2 ~ ("[" ~> expr9 <~ "]")
    lazy val expr2: PackratParser[Any] = callExpr | castExpr | subscriptExpr | fieldExpr | expr1

    lazy val expr3: PackratParser[Any] = {
      lazy val not = "!" ~> expr3
      lazy val minus = "-" ~> expr3
      not | minus | expr2
    }

    def makeParser(eqPrecdence: PackratParser[Any], higherPrecdence: PackratParser[Any])(ops: String*) = {
      val opsParsers = ops.toList map {
        (str) => eqPrecdence <~ str ~ higherPrecdence
      }
      opsParsers.reduceLeft(_ | _) | higherPrecdence
    }

    lazy val expr4: PackratParser[Any] = makeParser(expr4, expr3)("*", "/", "%")
    lazy val expr5: PackratParser[Any] = makeParser(expr5, expr4)("+", "-")
    lazy val expr6: PackratParser[Any] = makeParser(expr6, expr5)("<=", "<", ">=", ">")
    lazy val expr7: PackratParser[Any] = makeParser(expr7, expr6)("==", "!=")
    lazy val expr8: PackratParser[Any] = makeParser(expr8, expr7)("&&")
    lazy val expr9: PackratParser[Any] = makeParser(expr9, expr8)("||")
  }

  lazy val expr = Expressions.expr9

  def literal = numericLit | stringLit | arrayLit | recordLit | "true" | "false" | "null"

  def arrayLit = "[" ~ rep1sep(expr, ",") ~ "]"

  def recordLit = "(" ~ rep1sep(fieldLit, ",") ~ ")"

  def fieldLit = ident ~ "=" ~ expr
}

object TackParser {
  val reserved = List("bool", "else", "false", "for", "fun", "if", "in",
    "int", "null", "string", "true", "type", "void", "while", ":=", "->",
    "==", ">=", "<=", "&&", "||", "!=")

  val delimiters = List(
    "<=", ">=", "&&", "||", ":=", "->", "==", "!=",
    "(", ")", "{", "}", "[", "]",
    "+", "*", "/", "%", "_", "|", "&", "-", "=", ":", ">", "<",
    ",", ";", ".", ":", "#", "!")
}

sealed abstract class Expression

case class CallExpr(expr: Expression, rest: Expression*)

case class Literal(lit: String) extends Expression

case class InfixOp(op: String, left: Expression, right: Expression) extends Expression

case class UnaryOp(op: String, operand: Expression) extends Expression

case class PrefixOp(op: String, operand: Expression) extends Expression

sealed abstract class Statement

case class VarDef(id: String, expr: Expression) extends Statement

case class AssignStmt(left: Expression, right: Expression) extends Statement

case class BlockStmt(list: List[Statement]) extends Statement

case class CallStmt(expr: CallExpr) extends Statement

case class ForStmt(id: String, expr: Expression, stmt: BlockStmt) extends Statement

