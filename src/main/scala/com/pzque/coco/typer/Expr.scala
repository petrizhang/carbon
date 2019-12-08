package com.pzque.coco.typer

sealed abstract class Expr {
  var tpe: Type = TVar("?")
}

case class LitBool(value: Boolean) extends Expr {
  override def toString: String = value.toString
}

case class LitInt(value: Int) extends Expr {
  override def toString: String = value.toString
}

// VAR
case class Var(name: String) extends Expr

// APP
case class Apply(expr: Expr, arg: Expr) extends Expr

// ABS
case class Lambda(arg: String, body: Expr) extends Expr

// LET
case class Let(name: String, definition: Expr, body: Expr) extends Expr

// LETREC
case class LetRec(bindings: Array[(String, Expr)], body: Expr) extends Expr

