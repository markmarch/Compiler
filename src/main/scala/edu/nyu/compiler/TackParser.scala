package edu.nyu.compiler

import scala.util.parsing.combinator.{PackratParsers, ImplicitConversions}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
 * A simple parser that accepts programs written in Tack, a statically typed
 * language invented for the Compiler Construction course at NYU.
 *
 * This parser uses the parser combinator provided in the Scala library, and only
 * parse the input but does not generate AST.
 */
trait TackParser extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  // replace the default lexical with TackLexical
  override val lexical = new TackLexical

  // add delimiters
  lexical.delimiters ++= TackParser.delimiters

  // add keywords
  lexical.reserved ++= TackParser.reserved

  // productions
  def program = (funDef +)

  def funDef = ident ~ "=" ~ "fun" ~ funType ~ blockStmt

  def typ: Parser[Any] = arrayType | recordType | "bool" | "int" | "string"

  def arrayType = "[" ~ typ ~ "]"

  def recordType = "(" ~ repsep(fieldType, ",") ~ ")"

  def fieldType = ident ~ ":" ~ typ

  def funType = recordType ~ "->" ~ returnType

  def returnType = typ | "void"

  def stmt : Parser[Any] = varDef | assignStmt | blockStmt | callStmt | forStmt | ifStmt | returnStmt | whileStmt

  def varDef = ident ~ "=" ~ expr ~ ";"

  def assignStmt = expr ~ (":=" ~ expr) ~ ";"

  def blockStmt = "{" ~> rep(stmt) <~ "}"

  def callStmt = Expressions.callExpr ~ ";"

  def forStmt = "for" ~ ident ~ "in" ~ expr ~ blockStmt

  def ifStmt = "if" ~ expr ~ blockStmt ~ opt("else" ~ blockStmt)

  def returnStmt = "->" ~ opt(expr) ~ ";"

  def whileStmt = "while" ~ expr ~ blockStmt

  // wrap expressions into an object, expressionN, where the lower N the higher
  // precedence
  object Expressions {
    // parentheses and literals
    lazy val expr1: PackratParser[Any] = (ident | literal | "(" ~> expr9 <~ ")")

    // postfix call, cast , field and subscript
    lazy val callExpr = expr1 ~ ("(" ~> repsep(expr9, ",") <~ ")")
    lazy val castExpr = expr2 ~ (":" ~> typ)
    lazy val fieldExpr = expr2 ~ "." ~> ident
    lazy val subscriptExpr = expr2 ~ ("[" ~> expr9 <~ "]")
    lazy val expr2: PackratParser[Any] = callExpr | castExpr | subscriptExpr | fieldExpr | expr1

    // prefix operator ! and -
    lazy val expr3: PackratParser[Any] = {
      lazy val not = "!" ~> expr3
      lazy val minus = "-" ~> expr3
      not | minus | expr2
    }

    // create a parser using an expression with equal precedence and another expression
    // with higher precedence
    def makeParser(eqPrecedence: PackratParser[Any], higherPrecedence: PackratParser[Any])(ops: String*) = {
      // create a parser for each operators in the list
      val opsParsers = ops.toList map {
        (str) => eqPrecedence <~ str ~ higherPrecedence
      }
      // combine those parsers to form alternatives
      opsParsers.reduceLeft(_ | _) | higherPrecedence
    }

    // expressions with difference precedence
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

