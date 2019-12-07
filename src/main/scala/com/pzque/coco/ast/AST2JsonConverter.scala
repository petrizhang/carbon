package com.pzque.coco.ast

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods

import scala.collection.mutable

class AST2JsonConverter extends ASTVisitor[JValue] {

  def compact(ast: AST): String = JsonMethods.compact(JsonMethods.render(toJson(ast)))

  def pretty(ast: AST): String = JsonMethods.pretty(JsonMethods.render(toJson(ast)))

  def toJson(ast: AST): JValue = this.visit(ast)

  override protected def visitBooleanLiteral(ast: BooleanLiteral): JValue = {
    ("node" -> "BooleanLiteral") ~
      ("value" -> ast.value)
  }

  override protected def visitLongLiteral(ast: LongLiteral): JValue = {
    ("node" -> "LongLiteral") ~
      ("value" -> ast.value)
  }

  override protected def visitDoubleLiteral(ast: DoubleLiteral): JValue = {
    ("node" -> "DoubleLiteral") ~
      ("value" -> ast.value)
  }

  override protected def visitStringLiteral(ast: StringLiteral): JValue = {
    ("node" -> "StringLiteral") ~
      ("value" -> ast.value)
  }

  override protected def visitIdentifier(ast: Identifier): JValue = {
    ("node" -> "Identifier") ~
      ("name" -> ast.name)
  }

  override protected def visitBinaryExpr(ast: BinaryExpr, leftVisited: JValue, rightVisited: JValue): JValue = {
    ("node" -> "BinaryExpr") ~
      ("operator" -> ast.op.toString) ~
      ("left" -> leftVisited) ~
      ("right" -> rightVisited)
  }

  override protected def visitIfExpr(ast: IfExpr, conditionVisited: JValue, bodyVisited: JValue, elseBodyVisited: JValue): JValue = {
    ("node" -> "IfExpr") ~
      ("condition" -> conditionVisited) ~
      ("body" -> bodyVisited) ~
      ("else" -> elseBodyVisited)
  }

  override protected def visitDefine(ast: Define, identVisited: JValue, valueVisited: JValue): JValue = {
    ("node" -> "Define") ~
      ("ident" -> identVisited) ~
      ("value" -> valueVisited)
  }

  override protected def visitLambda(ast: Lambda, paramsVisited: mutable.Seq[JValue], bodyVisited: JValue): JValue = {
    ("node" -> "Lambda") ~
      ("params" -> paramsVisited) ~
      ("body" -> bodyVisited)
  }

  override protected def visitApply(ast: Apply, identVisited: JValue, argsVisited: mutable.Seq[JValue]): JValue = {
    ("node" -> "Apply") ~
      ("ident" -> identVisited) ~
      ("args" -> argsVisited)
  }

  override protected def visitBlock(ast: Block, exprListVisited: mutable.Seq[JValue]): JValue = {
    ("node" -> "Block") ~
      ("exprList" -> exprListVisited)
  }
}