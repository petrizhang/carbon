package com.pzque.coco.ast

import org.scalatest.FlatSpec

object ASTVisitorSpec

class BinaryExprVisitorSpec extends FlatSpec {

  val visitor: ASTVisitor[Any] = new ASTVisitor[Any] {
    override protected def visitDoubleLiteral(ast: DoubleLiteral): Any = ast.value

    override protected def visitBinaryExpr(ast: BinaryExpr, leftVisited: Any, rightVisited: Any): Any
    = ast.op.run(leftVisited, rightVisited)
  }

  "Number operations" should "be successful" in {
    val ast1 = BinaryExpr(Add, DoubleLiteral(1), DoubleLiteral(2))
    val ast2 = BinaryExpr(Add, DoubleLiteral(3), DoubleLiteral(4))
    val ast3 = BinaryExpr(Add, ast1, ast2)
    assert(visitor.visit(ast1) == 1 + 2)
    assert(visitor.visit(ast2) == 3 + 4)
    assert(visitor.visit(ast3) == (1 + 2) + (3 + 4))

    val ast4 = BinaryExpr(Sub, DoubleLiteral(2), DoubleLiteral(3.6))
    val ast5 = BinaryExpr(Mul, DoubleLiteral(4), DoubleLiteral(15))
    val ast6 = BinaryExpr(Div, ast4, ast5)
    assert(visitor.visit(ast6) == (2 - 3.6) / (4 * 15))
  }
}
