package com.pzque.coco.typer2

import substitution.Subst
import prelude._

sealed trait Type {
  def kind: Kind

  def typeVariables: Set[String]

  // Construct a function type
  def ->:(from: Type): Type = TAp(TAp(tArrow, from), this)

  // Apply a type with a argument `arg`
  def <<(arg: Type): Type = TAp(this, arg)
}

case class TVar(id: String) extends Type {
  override lazy val kind: Kind = Star

  override def typeVariables: Set[String] = Set(id)

  def +->(t: Type): Subst = Map(this -> t)

  override lazy val toString: String = id
}

case class TCon(id: String, body: Kind) extends Type {
  private var _formatter = formatters.normalFormatter

  def formatter: TApFormatter = _formatter

  def withFormatter(formatter: TApFormatter): TCon = {
    this._formatter = formatter
    this
  }

  override lazy val kind: Kind = body

  override def typeVariables: Set[String] = Set.empty[String]

  override lazy val toString: String = id
}

case class TAp(f: Type, arg: Type) extends Type {
  lazy val isFunction: Boolean = {
    f match {
      case TAp(con, _) if con == prelude.tArrow => true
      // TODO check if f could only be TCon
      case _ => false
    }
  }

  override lazy val kind: Kind = {
    f.kind match {
      case KFun(_, k) => k
      case _ => throw new Error("expected a KFunc instance")
    }
  }

  override def typeVariables: Set[String] = f.typeVariables ++ arg.typeVariables

  override lazy val toString: String = {
    f match {
      case TAp(con: TCon, _) => con.formatter.format(this)
      case _ => s"$f $arg"
    }
  }
}

case class TGen(value: Int) extends Type {
  def kind: Kind = ???

  override def typeVariables: Set[String] = ???
}

