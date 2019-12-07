package com.pzque.coco.ast

import scala.collection.mutable


trait ASTVisitor[T] {

  case class Elem[E](ast: AST,
                     visitedChildren: mutable.ArrayBuffer[E] = mutable.ArrayBuffer.empty[E])

  /** The evaluation stack */
  private val stack: mutable.Stack[Elem[T]] = mutable.Stack()

  /** The final evaluation result */
  private var result: Option[T] = None

  /** *
    * Main entry of the visitor.
    *
    * @param ast
    * @return
    */
  def visit(ast: AST): T = {
    stack.push(Elem[T](ast))

    while (stack.nonEmpty) {
      val currentNode = stack.top
      val visitedChildren = currentNode.visitedChildren
      // Get next child of current node
      val nextChild = currentNode.ast.getChild(visitedChildren.length)
      // If next child is None, reduce current node to type T
      if (nextChild.isEmpty) {
        val result = reduce(currentNode.ast, visitedChildren)
        emitResult(result)
      } else {
        // Push next child to stack to visit it
        stack.push(Elem[T](nextChild.get))
      }
    }
    require(result.nonEmpty)
    result.get
  }

  private def reduce(ast: AST, visitedChildren: mutable.ArrayBuffer[T]): T = {
    ast match {
      case x: BooleanLiteral => visitBooleanLiteral(x)
      case x: LongLiteral => visitLongLiteral(x)
      case x: DoubleLiteral => visitDoubleLiteral(x)
      case x: StringLiteral => visitStringLiteral(x)
      case x: Identifier => visitIdentifier(x)
      case x: BinaryExpr => reduceBinaryExpr(x, visitedChildren)
      case x: IfExpr => reduceIfExpr(x, visitedChildren)
      case x: Define => reduceDefine(x, visitedChildren)
      case x: Lambda => reduceLambda(x, visitedChildren)
      case x: Apply => reduceApply(x, visitedChildren)
      case x: Block => reduceBlock(x, visitedChildren)
    }
  }

  private def reduceBinaryExpr(ast: BinaryExpr, visitedChildren: mutable.ArrayBuffer[T]): T = {
    require(ast.childrenCount == visitedChildren.length)

    visitBinaryExpr(ast, visitedChildren(0), visitedChildren(1))
  }

  private def reduceIfExpr(ast: IfExpr, visitedChildren: mutable.ArrayBuffer[T]): T = {
    require(ast.childrenCount == visitedChildren.length)

    visitIfExpr(ast, visitedChildren(0), visitedChildren(1), visitedChildren(2))
  }

  private def reduceDefine(ast: Define, visitedChildren: mutable.ArrayBuffer[T]): T = {
    require(ast.childrenCount == visitedChildren.length)

    visitDefine(ast, visitedChildren(0), visitedChildren(1))
  }

  private def reduceLambda(ast: Lambda, visitedChildren: mutable.ArrayBuffer[T]): T = {
    require(ast.childrenCount == visitedChildren.length)

    val paramNum = ast.params.length
    visitLambda(ast, visitedChildren, visitedChildren(paramNum))
  }

  private def reduceApply(ast: Apply, visitedChildren: mutable.ArrayBuffer[T]): T = {
    require(ast.childrenCount == visitedChildren.length)

    visitApply(ast, visitedChildren.head, visitedChildren.tail)
  }

  private def reduceBlock(ast: Block, visitedChildren: mutable.ArrayBuffer[T]): T = {
    require(ast.childrenCount == visitedChildren.length)

    visitBlock(ast, visitedChildren)
  }


  /** *
    * Emit visited result to stack top.
    * If stack is empty, save the result to [[result]]
    *
    * @param tmpResult
    */
  private def emitResult(tmpResult: => T): Unit = {
    stack.pop()
    if (stack.isEmpty) {
      this.result = Some(tmpResult)
    } else {
      if (!stack.top.ast.isLeaf) {
        stack.top.visitedChildren += tmpResult
      } else {
        throw new RuntimeException("Leaf node could only be on stack top.")
      }
    }
  }

  protected def visitBooleanLiteral(ast: BooleanLiteral): T = throw new NotImplementedError()

  protected def visitLongLiteral(ast: LongLiteral): T = throw new NotImplementedError()

  protected def visitDoubleLiteral(ast: DoubleLiteral): T = throw new NotImplementedError()

  protected def visitStringLiteral(ast: StringLiteral): T = throw new NotImplementedError()

  protected def visitIdentifier(ast: Identifier): T = throw new NotImplementedError()

  protected def visitBinaryExpr(ast: BinaryExpr, leftVisited: T, rightVisited: T): T = throw new NotImplementedError()

  protected def visitIfExpr(ast: IfExpr, conditionVisited: T, bodyVisited: T, elseBodyVisited: T): T = throw new NotImplementedError()

  protected def visitDefine(ast: Define, identVisited: T, valueVisited: T): T = throw new NotImplementedError()

  protected def visitLambda(ast: Lambda, paramsVisited: mutable.Seq[T], bodyVisited: T): T = throw new NotImplementedError()

  protected def visitApply(ast: Apply, identVisited: T, argsVisited: mutable.Seq[T]): T = throw new NotImplementedError()

  protected def visitBlock(ast: Block, exprListVisited: mutable.Seq[T]): T = throw new NotImplementedError()
}
