package edu.nyu.compiler

import scala.util.parsing.combinator.{PackratParsers, ImplicitConversions}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
 * A simple parser that accepts programs written in Tack, a statically typed
 * language invented for the Compiler Construction course at NYU.
 *
 */
trait TackParser extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  // replace the default lexical with TackLexical
  override val lexical = new TackLexical

  // add delimiters
  lexical.delimiters ++= TackParser.delimiters

  // add keywords
  lexical.reserved ++= TackParser.reserved

  // productions
  def program : Parser[Program] = (funDef +) ^^ { (funList : List[FunDef]) => Program(funList)}

  def funDef : Parser[FunDef] = ident ~ "=" ~ "fun" ~ funType ~ blockStmt  ^^ {
    case id ~ "=" ~ "fun" ~ typ ~ bs => 
      bs.stmtList.last match {
        case _ : ReturnStmt => FunDef(FunId(id), typ, bs)
        case _ => FunDef(FunId(id), typ, BlockStmt(bs.stmtList ::: List(ReturnStmt(None))))
      }
  }

  def typ: Parser[Type] = arrayType | recordType| "bool" ^^^ {PrimitiveType("bool")} |
    "int" ^^^ {PrimitiveType("int")} | "string" ^^^ {PrimitiveType("string")}


  def arrayType : Parser[ArrayType]= "[" ~> typ <~ "]" ^^ {(t : Type) => ArrayType(t)}

  def recordType : Parser[RecordType] = "(" ~> repsep(fieldType, ",") <~ ")" ^^
    { (fList : List[FieldType]) => RecordType(fList)}

  def fieldType : Parser[FieldType] = ident ~ ":" ~ typ ^^
    {case id ~ ":" ~ t => FieldType(FieldId(id), t)}

  def funType : Parser[FunType] = recordType ~ "->" ~ returnType ^^
    {case from ~ "->" ~ to => FunType(from, to)}

  def returnType : Parser[Type] = typ | "void" ^^^ {PrimitiveType("void")}

  def stmt : Parser[Stmt] = varDef | assignStmt | blockStmt | callStmt | forStmt | ifStmt | returnStmt | whileStmt

  def varDef : Parser[VarDef]= ident ~ "=" ~ expr ~ ";" ^^
    {case id ~ "=" ~ e ~ ";" => VarDef(VarId(id), e)}

  def assignStmt : Parser[AssignStmt] = expr ~ (":=" ~> expr)  ~ ";" ^^
    { case left ~ right ~ ";" => AssignStmt(left, right)}

  def blockStmt : Parser[BlockStmt] = "{" ~> rep(stmt) <~ "}" ^^
    {(stmtList : List[Stmt]) => BlockStmt(stmtList)}

  def callStmt : Parser[CallStmt] = Expressions.callExpr ~ ";" ^^
    {case e ~ ";" => CallStmt(e)}

  def forStmt : Parser[ForStmt] = "for" ~ ident ~ "in" ~ expr ~ blockStmt ^^
    { case "for" ~ id ~ "in" ~ e ~ bs => ForStmt(VarId(id),e,bs)}

  def ifStmt : Parser[IfStmt] = "if" ~ expr ~ blockStmt ~ opt("else" ~> blockStmt)^^
    { case "if" ~ e ~ bs ~ optBs => IfStmt(e, bs, optBs)}

  def returnStmt : Parser[ReturnStmt] = "->" ~ opt(expr)  ~ ";"  ^^
    { case "->" ~ optExpr ~ ";" => ReturnStmt(optExpr)}

  def whileStmt = "while" ~ expr ~ blockStmt ^^ { case "while" ~ e ~ bs => WhileStmt(e, bs)}

  // wrap expressions into an object, expressionN, where the lower N the higher
  // precedence
  object Expressions {
    // parentheses and literals
    lazy val expr1: PackratParser[Expression] = ident ^^ {(id : String) => VarId(id)} | literal |
      "(" ~> expr9 <~ ")" ^^ { (e : Expression) => ParenExpr(e)}
    
    // postfix call, cast , field and subscript
    lazy val callExpr : PackratParser[CallExpr] = ident ~ ("(" ~> repsep(expr9, ",") <~ ")") ^^
      { case callee ~ paramList => CallExpr(FunId(callee), paramList)}
    lazy val castExpr : PackratParser[CastExpr] = expr2 ~ (":" ~> typ) ^^ { case e ~ t => CastExpr(e, t)}
    lazy val fieldExpr : PackratParser[FieldExpr] = expr2 ~ "." ~ ident ^^ { case e ~ "." ~ id => FieldExpr(e, FieldId(id))}
    lazy val subscriptExpr : PackratParser[SubscriptExpr] = expr2 ~ ("[" ~> expr9 <~ "]") ^^ {case left ~ right => SubscriptExpr(left, right)}
    lazy val expr2: PackratParser[Expression] = callExpr | castExpr | subscriptExpr | fieldExpr | expr1

    // prefix operator ! and -
    lazy val expr3: PackratParser[Expression] = {
      lazy val not = "!" ~> expr3 ^^ { PrefixExpr("!", _)}
      lazy val minus = "-" ~> expr3 ^^ {PrefixExpr("-", _)}
      not | minus | expr2
    }

    // create a parser using an expression with equal precedence and another expression
    // with higher precedence
    def makeParser(eqPrecedence: PackratParser[Expression], higherPrecedence: PackratParser[Expression])(ops: String*) = {
      // create a parser for each operators in the list
      val opsParsers = ops.toList map {
        (str) => eqPrecedence ~ str ~ higherPrecedence ^^ { case left ~ op ~ right => InfixExpr(op, left,right)}
      }
      // combine those parsers to form alternatives
      opsParsers.reduceLeft(_ | _) | higherPrecedence
    }

    // expressions with difference precedence
    lazy val expr4: PackratParser[Expression] = makeParser(expr4, expr3)("*", "/", "%")
    lazy val expr5: PackratParser[Expression] = makeParser(expr5, expr4)("+", "-")
    lazy val expr6: PackratParser[Expression] = makeParser(expr6, expr5)("<=", "<", ">=", ">")
    lazy val expr7: PackratParser[Expression] = makeParser(expr7, expr6)("==", "!=")
    lazy val expr8: PackratParser[Expression] = makeParser(expr8, expr7)("&&")
    lazy val expr9: PackratParser[Expression] = makeParser(expr9, expr8)("||")
  }

  lazy val expr = Expressions.expr9

  def literal = numericLit ^^ { (s : String) => IntLit(s.toInt)} |
    stringLit ^^ { (s : String) => StringLit(s) } | arrayLit | recordLit |
    "true" ^^^ { BoolLit(true) } | "false" ^^^ { BoolLit(false) } | "null" ^^^ {NullLit()}

  def arrayLit = "[" ~> repsep(expr, ",") <~ "]" ^^
    { (exprList : List[Expression]) => ArrayLit(exprList)}

  def recordLit = "(" ~> repsep(fieldLit, ",") <~ ")"  ^^
    { (fieldLitList : List[FieldLit]) => RecordLit(fieldLitList)}

  def fieldLit = ident ~ "=" ~ expr ^^ { case id ~ "=" ~ e => FieldLit(FieldId(id), e)}
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

