package com.pzque.coco

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox

object Eval extends App {
  val a = 1

  def eval(code: String): Any = {
    val mirror = currentMirror
    val tb = mirror.mkToolBox()
    val tree = tb.parse(
      s"""import com.pzque.sky.ast._
         |$code
        """.stripMargin)
    println(tb.typecheck(tree))
    tb.eval(tree)
  }

  val code =
    """
      | val a = LongLiteral(10);
      | println(a)
      |""".stripMargin
  eval(code)
}