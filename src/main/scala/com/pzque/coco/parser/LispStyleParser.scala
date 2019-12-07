package com.pzque.coco.parser

import com.pzque.coco.ast._

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.Reader

class LispStyleParser extends StdTokenParsers
  with SkyParser {
  type Tokens = StdLexical
  val lexical = new StdLexical

  lexical.reserved ++= List("define", "lambda", "true", "false", "if", "for", "new")
  lexical.delimiters ++= List("[", "]", "(", ")", "{", "}",
    ":", ";", ".", ",", "=",
    "+", "-", "*", "/", "\"", "%",
    "==", "<", "<=", ">", ">=",
    "&&", "!", "||")

  override def parse(reader: Reader[Char]): Either[String, AST] = {
    parseFromScanner(new lexical.Scanner(reader))
  }

  override def parse(code: String): Either[String, AST] = {
    parseFromScanner(new lexical.Scanner(code))
  }

  def parseFromScanner(scanner: lexical.Scanner): Either[String, AST] = {
    val parsed = programParser(scanner)
    if (parsed.successful) {
      Right(parsed.get)
    } else {
      Left(parsed.toString)
    }
  }

  def programParser: Parser[AST] = rep1(sExprParser) ^^ {
    exprList => Block(exprList: _*)
  }

  def exprListParser: Parser[AST] = rep1(exprParser) ^^ {
    exprList => Block(exprList: _*)
  }

  def exprParser: Parser[AST] = (
    stringLit ^^ { x: String => StringLiteral(x) }
      | keyword("true") ^^ { _ => BooleanLiteral(true) }
      | keyword("false") ^^ { _ => BooleanLiteral(false) }
      | numericLit ^^ { x: String => DoubleLiteral(x.toDouble) }
      | identifierParser
      | sExprParser
      | failure("Failed to parse expression.")
    )

  def sExprParser: Parser[AST] =
    builtinParser |
      "(" ~ ident ~ rep1(exprParser) ~ ")" ^^ {
        case "(" ~ name ~ args ~ ")" => Apply(Identifier(name), args: _*)
      } |
      failure("Failed to parse S-Expression.")

  def identifierParser: Parser[Identifier] = ident ^^ { x: String => Identifier(x) }

  def identifierListParser: Parser[Seq[Identifier]] =
    "(" ~> rep1(identifierParser) <~ ")"

  def varDefParser: Parser[AST] = "(" ~ keyword("define") ~ ident ~ exprParser ~ ")" ^^ {
    case "(" ~ _ ~ name ~ body ~ ")" => Define(Identifier(name), body)
  }

  def funcDefParser: Parser[AST] =
    "(" ~ keyword("define") ~ identifierListParser ~ exprListParser ~ ")" ^^ {
      case "(" ~ _ ~ idList ~ body ~ ")" => {
        val name = idList.head
        val params = idList.tail
        Define(name, Lambda(params, body))
      }
    }

  def defineParser: Parser[AST] = varDefParser | funcDefParser

  def ifExprParser: Parser[AST] =
    "(" ~ keyword("if") ~ exprParser ~ exprParser ~ exprParser ~ ")" ^^ {
      case "(" ~ _ ~ cont ~ body ~ elseBody ~ ")" => IfExpr(cont, body, elseBody)
    }

  def lambdaParser: Parser[AST] =
    "(" ~ keyword("lambda") ~ identifierListParser ~ exprParser ~ ")" ^^ {
      case "(" ~ _ ~ params ~ body ~ ")"
      => Lambda(params, body)
    }

  def builtinParser: Parser[AST] = defineParser | ifExprParser | lambdaParser | binaryExprParser

  def binaryExprParser: Parser[AST] =
    "(" ~ binaryOpParser ~ exprParser ~ exprParser ~ ")" ^^ {
      case "(" ~ op ~ left ~ right ~ ")" => BinaryExpr(op, left, right)
    }

  def binaryOpParser: Parser[BinaryOperator] =
    "+" ^^^ Add |
      "-" ^^^ Sub |
      "*" ^^^ Mul |
      "/" ^^^ Div |
      "&&" ^^^ And |
      "||" ^^^ Or |
      "<" ^^^ Lt |
      "<=" ^^^ Leq |
      "==" ^^^ Eq |
      ">=" ^^^ Geq |
      ">=" ^^^ Gt
}
