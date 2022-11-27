package imp

import java.nio.file.{Files, Paths}
import scala.util.parsing.combinator.JavaTokenParsers

object ImpParser extends JavaTokenParsers {

  // parse functions

  def parsePgrm(filename: String): ParseResult[Statement] = {
    try {
      val source = Files.readString(Paths.get(filename))
      parseStmt(source)
    } catch {
      case e: Exception => throw new Exception(e)
    }
  }

  def parseStmt(input: String): ParseResult[Statement] =
    parse(stmt, input)

  def parse[R](parser: Parser[R], input: String): ParseResult[R] =
    super.parse(parser, input)


  // Statement parser

  def stmt: Parser[Statement] = assignment | ifStmt | whileStmt | blockStmt | printStmt

  def assignment: Parser[AssnStmt] =
    variable ~ ":=" ~ expr <~ ";" ^^ { case v ~ _ ~ e => AssnStmt(v, e) }

  def ifStmt: Parser[IfStmt] =
    "if" ~> expr ~ stmt ~ opt("else" ~> stmt) ^^ {
      case c ~ t ~ Some(e) => IfStmt(c, t, e)
      case c ~ t ~ None => IfStmt(c, t, Skip)
    }

  def whileStmt: Parser[WhileStmt] =
    "while" ~> expr ~ stmt ^^ { case c ~ s => WhileStmt(c, s) }

  def blockStmt: Parser[BlockStmt] =
    "{" ~> rep(stmt) <~ "}" ^^ { case l => BlockStmt(l) }

  def printStmt: Parser[PrintStmt] =
    "print" ~> "(" ~> stringLiteral ~ "," ~ expr <~ ")" <~ ";" ^^ {
      case s ~ _ ~ v => PrintStmt(s.substring(1, s.length - 1), v)
    }


  // Expression parser

  def expr: Parser[Expr] =
    literal | readIntExpr | readBoolExpr | variable | binExpr | unyExpr

  def literal: Parser[Val] =
    ("true" | "false" | decimalNumber) ^^ {
      case "true" => BoolVal(true)
      case "false" => BoolVal(false)
      case s =>
        try {
          IntVal(s.toInt)
        } catch {
          case e: Exception => IntVal(Int.MaxValue)
        }
    }

  def variable: Parser[Var] = ident ^^ { n => Var(n) }

  def binExpr: Parser[BinExpr] =
    "(" ~> expr ~ binOp ~ expr <~ ")" ^^ { case l ~ op ~ r => BinExpr(op, l, r) }

  def unyExpr: Parser[UnyExpr] =
    "(" ~> unyOp ~ expr <~ ")" ^^ { case op ~ s => UnyExpr(op, s) }

  def binOp =
    "+" | "*" | "-" | "/" | "%" | "<=" | ">=" | "==" | "!=" | "<" | ">" | "&&" | "||"

  def unyOp =
    "!" | "-"

  def readIntExpr: Parser[ReadInt] =
    "readInt" ~> "(" ~> stringLiteral <~ ")" ^^ {
      case prompt => ReadInt(prompt.substring(1, prompt.length - 1))
    }

  def readBoolExpr: Parser[ReadBool] =
    "readBool" ~> "(" ~> stringLiteral <~ ")" ^^ {
      case prompt => ReadBool(prompt.substring(1, prompt.length - 1))
    }

}
