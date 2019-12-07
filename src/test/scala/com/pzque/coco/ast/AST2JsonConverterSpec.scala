package com.pzque.coco.ast

import com.pzque.coco.parser.LispStyleParser
import org.scalatest.FlatSpec

class AST2JsonConverterSpec extends FlatSpec {

  val parser = new LispStyleParser
  val formatter = new AST2JsonConverter

  it should "ok" in {
    val ast1 = BinaryExpr(Add, DoubleLiteral(1), DoubleLiteral(2))
    val ast2 = BinaryExpr(Mul, ast1, ast1)
    println(formatter.pretty(ast2))

    val code3 =
      """
        |(define a 1)
        |(define b 2)
        |(define (add x y)
        |  (+ x y))
      """.stripMargin
    val ast3 = parser.parse(code3).right.get
    println(formatter.pretty(ast3))
  }
}
