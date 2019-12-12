package com.pzque.coco.typer2

import prelude._
import unify._
import typeclass._
import types._
import implicits._

object Main extends App {
  def test1(): Unit = {
    val b = tInt ->: (tList << "a")
    val c = tList << tInt
    val d = tList << "a"
    val e = tInt ->: c
    val f = "a" ->: d
    val g = mgu(e, f)
    val h = typeImplTypes.applySubst(g, e)
    val i = typeImplTypes.applySubst(g, f)

    println(b)
    println(c)
    println(d)
    println(e)
    println(f)
    println(g)
    println(h)
    println(i)
  }

  def test2(): Unit = {
    val n = tBoolean ->: tInt ->: tChar

    val o = (tBoolean ->: tInt) ->: tChar

    val q = (tChar ->: tInt) ->: "b" ->: "a"
    println(q.typeVariables)

    val p = List(Pred("Num", "a"), Pred("Num", "b")) :=> ("a" ->: tInt)
    println(p)
  }

  def test3(): Unit = {
    val a: Class = Class(
      ab("Eq"),
      ab(
        List() :=> ("Ord" $ tUnit),
        List() :=> ("Ord" $ tChar),
        List() :=> ("Ord" $ tInt),
        List("Ord" $ "a", "Ord" $ "b") :=>
          ("Ord" $ pair("a", "b"))
      )
    )
    println(a)
  }

  def test4(): Unit = {
    val env = new ClassEnv
    env.addClass("Eq", ab)
      .addClass("Ord", ab("Eq"))
      .addClass("Show", ab)
      .addClass("Read", ab)
      .addClass("Bounded", ab)
      .addClass("Enum", ab)
      .addClass("Functor", ab)
      .addClass("Monad", ab)
    println(env)
  }

  test4()
}