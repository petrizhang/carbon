package com.pzque.coco.ast

/**
  * The base AST(Abstract Syntax Tree) class.
  */
sealed trait AST {
  override def toString: String = (new AST2JsonConverter).pretty(this)

  /**
    * Get the count of children.
    *
    * @return
    */
  def childrenCount: Int


  def getChild(i: Int): Option[AST] = {
    require(i >= 0, "Child index must be larger than zero.")
    getChildImpl(i)
  }

  /**
    * Get the i'th child.
    *
    * @param i
    */
  protected def getChildImpl(i: Int): Option[AST]

  /**
    * Whether the ast node is a leaf node or not.
    *
    * @return
    */
  def isLeaf: Boolean
}

trait Literal extends AST {
  /**
    * Get the count of children.
    *
    * @return
    */
  override def childrenCount: Int = 0

  /**
    * Get the i'th child.
    *
    * @param i
    */
  override protected def getChildImpl(i: Int): Option[AST] = None

  /**
    * Whether the ast node is a leaf node or not.
    *
    * @return
    */
  override def isLeaf: Boolean = true
}

case class BooleanLiteral(value: Boolean) extends Literal

case class LongLiteral(value: Long) extends Literal

case class DoubleLiteral(value: Double) extends Literal

case class StringLiteral(value: String) extends Literal

case class Identifier(name: String) extends AST {
  /**
    * Get the count of children.
    *
    * @return
    */
  override def childrenCount: Int = 0

  /**
    * Get the i'th child.
    *
    * @param i
    */
  override protected def getChildImpl(i: Int): Option[AST] = None

  /**
    * Whether the ast node is a leaf node or not.
    *
    * @return
    */
  override def isLeaf: Boolean = true
}

case class BinaryExpr(op: BinaryOperator, left: AST, right: AST) extends AST {
  /**
    * Get the count of children.
    *
    * @return
    */
  override def childrenCount: Int = 2

  /**
    * Get the i'th child.
    *
    * @param i
    */
  override protected def getChildImpl(i: Int): Option[AST] = {
    if (i == 0) Some(left)
    else if (i == 1) Some(right)
    else None
  }

  /**
    * Whether the ast node is a leaf node or not.
    *
    * @return
    */
  override def isLeaf: Boolean = false
}

case class IfExpr(condition: AST, body: AST, elseBody: AST) extends AST {
  /**
    * Get the count of children.
    *
    * @return
    */
  override def childrenCount: Int = 3

  /**
    * Get the i'th child.
    *
    * @param i
    */
  override protected def getChildImpl(i: Int): Option[AST] = {
    if (i == 0) Some(condition)
    else if (i == 1) Some(body)
    else if (i == 2) Some(elseBody)
    else None
  }

  /**
    * Whether the ast node is a leaf node or not.
    *
    * @return
    */
  override def isLeaf: Boolean = false
}

case class Define(ident: Identifier, value: AST) extends AST {
  /**
    * Get the count of children.
    *
    * @return
    */
  override def childrenCount: Int = 2

  /**
    * Get the i'th child.
    *
    * @param i
    */
  override protected def getChildImpl(i: Int): Option[AST] = {
    if (i == 0) Some(ident)
    else if (i == 1) Some(value)
    else None
  }

  /**
    * Whether the ast node is a leaf node or not.
    *
    * @return
    */
  override def isLeaf: Boolean = false
}

case class Lambda(params: Seq[Identifier], body: AST) extends AST {
  /**
    * Get the count of children.
    *
    * @return
    */
  override def childrenCount: Int = params.length + 1

  /**
    * Get the i'th child.
    *
    * @param i
    */
  override protected def getChildImpl(i: Int): Option[AST] = {
    if (i < params.length) Some(params(i))
    else if (i == params.length) Some(body)
    else None
  }

  /**
    * Whether the ast node is a leaf node or not.
    *
    * @return
    */
  override def isLeaf: Boolean = false
}

case class Apply(variable: Identifier, args: AST*) extends AST {
  /**
    * Get the count of children.
    *
    * @return
    */
  override def childrenCount: Int = 1 + args.length

  /**
    * Get the i'th child.
    *
    * @param i
    */
  override protected def getChildImpl(i: Int): Option[AST] = {
    if (i == 0) Some(variable)
    else if (i - 1 < args.length) Some(args(i - 1))
    else None
  }

  /**
    * Whether the ast node is a leaf node or not.
    *
    * @return
    */
  override def isLeaf: Boolean = false
}

case class Block(exprList: AST*) extends AST {
  /**
    * Get the count of children.
    *
    * @return
    */
  override def childrenCount: Int = exprList.length

  /**
    * Get the i'th child.
    *
    * @param i
    */
  override protected def getChildImpl(i: Int): Option[AST] = {
    if (i < exprList.length) Some(exprList(i))
    else None
  }

  /**
    * Whether the ast node is a leaf node or not.
    *
    * @return
    */
  override def isLeaf: Boolean = false
}


// case class CallCC(funcName: String) extends Expr
