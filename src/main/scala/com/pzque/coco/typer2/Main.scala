package com.pzque.coco.typer2

import prelude._
import unify._
import substitution._
import typeclass._
import types._

object Main extends App {
  val t0 = System.currentTimeMillis()
  //TAp (TAp tArrow tInt) (TAp tList (TVar (Tyvar "a" Star)))
  val a = TAp(TAp(tArrow, tInt), TAp(tList, TVar("a", Star)))
  val b = tInt >> (tList << TVar("a", Star))
  val c = tList << tInt
  val d = tList << TVar("a", Star)
  val e = tInt >> c
  val f = TVar("c", Star) >> d
  val g = mgu(e, f)
  val h = typeImplTypes.applySubst(g, e)
  val i = typeImplTypes.applySubst(g, f)
  val t1 = System.currentTimeMillis()
  println(s"Cost: ${t1 - t0}ms")

  println(b)
  println(b)
  println(h, i)
  val p = List(Pred("Num", TVar("a", Star)), Pred("Num", TVar("b", Star))) :=> (TVar("a", Star) >> tInt)

  println(p)
}