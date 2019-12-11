package com.pzque.coco.typer2

import substitution.Subst
import prelude._

trait TApFormatter {
  def format(f: TCon, arg: Type): String

  def formatEnd(formatted: String): String
}

object formatters {
  val funcFormatter: TApFormatter = new TApFormatter {
    override def format(f: TCon, arg: Type): String = s"$arg ->"

    override def formatEnd(formatted: String): String = formatted
  }

  val normalFormatter: TApFormatter = new TApFormatter {
    override def format(f: TCon, arg: Type): String = s"$f $arg"

    override def formatEnd(formatted: String): String = formatted
  }

  val listFormatter: TApFormatter = new TApFormatter {
    override def format(f: TCon, arg: Type): String = arg.toString

    override def formatEnd(formatted: String): String = s"[$formatted]"
  }

  val tupleFormatter: TApFormatter = new TApFormatter {
    override def format(f: TCon, arg: Type): String = s"$arg,"

    override def formatEnd(formatted: String): String = s"($formatted)"
  }
}

sealed trait Type {
  def kind: Kind

  // Construct a function type
  // must be used as a >> (b >> (c >> ...)) !!!
  def >>(to: Type): Type = TAp(TAp(tArrow, this), to)

  // Apply a type with a argument `arg`
  def <<(arg: Type): Type = TAp(this, arg)
}

case class TVar(id: String, body: Kind) extends Type {
  override lazy val kind: Kind = body

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

  override lazy val toString: String = id
}

case class TAp(f: Type, arg: Type) extends Type {

  override lazy val kind: Kind = {
    f.kind match {
      case KFun(_, k) => k
      case _ => throw new Error("expected a KFunc instance")
    }
  }

  override lazy val toString: String = {
    f match {
      case con: TCon => con.formatter.formatEnd(con.formatter.format(con, arg))
      case _ => s"$f $arg"
    }
  }
}

case class TGen(value: Int) extends Type {
  def kind: Kind = ???
}

