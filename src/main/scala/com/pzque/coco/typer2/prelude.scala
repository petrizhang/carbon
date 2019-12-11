package com.pzque.coco.typer2

object prelude {
  val tUnit: Type = TCon("()", Star)
  val tChar: Type = TCon("Char", Star)
  val tInt: Type = TCon("Int", Star)
  val tBoolean: Type = TCon("Boolean", Star)
  val tArrow: Type = TCon("(->)", Star ~> (Star ~> Star)).withFormatter(formatters.funcFormatter)
  val tList: Type = TCon("[]", Star ~> Star).withFormatter(formatters.listFormatter)
  val tPair: Type = TCon("(,)", Star ~> (Star ~> Star))

  def list(t: Type): Type = tList << t

  def pair(a: Type, b: Type): Type = tPair << a << b
}