package com.pzque.coco.ast

sealed abstract class Operator

abstract class BinaryOperator(name: String) extends Operator {
  def run(left: Any, right: Any): Any

  override def toString: String = name
}

/// +
object Add extends BinaryOperator("+") {
  override def run(left: Any, right: Any): Double = {
    left.asInstanceOf[Double] + right.asInstanceOf[Double]
  }
}

/// -
object Sub extends BinaryOperator("-") {
  override def run(left: Any, right: Any): Double = {
    left.asInstanceOf[Double] - right.asInstanceOf[Double]
  }
}

/// *
object Mul extends BinaryOperator("*") {
  override def run(left: Any, right: Any): Double = {
    left.asInstanceOf[Double] * right.asInstanceOf[Double]
  }
}

/// /
object Div extends BinaryOperator("/") {
  override def run(left: Any, right: Any): Double = {
    left.asInstanceOf[Double] / right.asInstanceOf[Double]
  }
}

/// >
object Gt extends BinaryOperator(">") {
  override def run(left: Any, right: Any): Boolean = {
    left.asInstanceOf[Double] > right.asInstanceOf[Double]
  }
}

/// >=
object Geq extends BinaryOperator(">=") {
  override def run(left: Any, right: Any): Boolean = {
    left.asInstanceOf[Double] >= right.asInstanceOf[Double]
  }
}

/// ==
object Eq extends BinaryOperator("==") {
  override def run(left: Any, right: Any): Boolean = {
    left.asInstanceOf[Double] == right.asInstanceOf[Double]
  }
}

/// <=
object Leq extends BinaryOperator("<=") {
  override def run(left: Any, right: Any): Boolean = {
    left.asInstanceOf[Double] <= right.asInstanceOf[Double]
  }
}

/// <
object Lt extends BinaryOperator("<") {
  override def run(left: Any, right: Any): Boolean = {
    left.asInstanceOf[Double] < right.asInstanceOf[Double]
  }
}

/// and
object And extends BinaryOperator("&&") {
  override def run(left: Any, right: Any): Boolean = {
    left.asInstanceOf[Boolean] && right.asInstanceOf[Boolean]
  }
}

/// or
object Or extends BinaryOperator("||") {
  override def run(left: Any, right: Any): Boolean = {
    left.asInstanceOf[Boolean] || right.asInstanceOf[Boolean]
  }
}

object Operator {
  private val mapping: Map[String, BinaryOperator] = Map(
    "+" -> Add,
    "-" -> Sub,
    "*" -> Mul,
    "/" -> Div,
    "<" -> Lt,
    "<=" -> Leq,
    "==" -> Eq,
    ">=" -> Geq,
    ">" -> Gt,
    "&&" -> And,
    "or" -> Or
  )

  def getBinaryOperator(op: String): Option[BinaryOperator] = mapping.get(op)
}