package com.pzque.coco.typer2

import prelude._
import unify._
import typeclass._
import types._

object Main extends App {
  val b = tInt ->: (tList << "a")
  val c = tList << tInt
  val d = tList << "a"
  val e = tInt ->: c
  val f = "a" ->: d
  val g = mgu(e, f)
  val h = typeImplTypes.applySubst(g, e)
  val i = typeImplTypes.applySubst(g, f)

  val n = tBoolean ->: tInt ->: tChar

  val o = (tBoolean ->: tInt) ->: tChar

  val q = (tChar ->: tInt) ->: tBoolean ->: tInt

  val p = List(Pred("Num", "a"), Pred("Num", "b")) :=> ("a" ->: tInt)
  println(p)
}