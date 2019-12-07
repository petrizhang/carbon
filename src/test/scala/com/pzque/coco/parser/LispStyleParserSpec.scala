package com.pzque.coco.parser

import org.scalatest.FlatSpec

class LispStyleParserSpec extends FlatSpec {

  val parser = new LispStyleParser

  "Binary expressions" should "be parsed" in {
    val code =
      """
        |(+ 1 1)
        |(- 1 1)
        |(* 1 1)
        |(/ 1 1)
        |(< 1 1)
        |(<= 1 1)
        |(== 1 1)
        |(>= 1 1)
        |(> 1 1)
        |(&& 1 1)
        |(|| 1 1)
      """.stripMargin
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }

  "If expression" should "be parsed" in {
    val code = "(if (== a 1) a 1)"
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }

  "Variable definition 0" should "be parsed" in {
    val code = "(define a (+ x 1))"
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }

  "Variable definition 1" should "be parsed" in {
    val code = "(define a 11)"
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }

  "Lambda expression0" should "be parsed" in {
    val code = "(lambda (x) (+ x 1))"
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }


  "Lambda expression without ()" should "not be parsed" in {
    val code = "(lambda x (+ x 1))"
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isLeft)
  }

  "Function definition with one parameter" should "be parsed" in {
    val code = "(define (f x) (+ x 1))"
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }

  "Function definition without parameters" should "be parsed" in {
    val code = "(define (f) (+ x 1))"
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }

  "Multiple S-Expressions" should "be parsed" in {
    val code =
      """
        |(define (add x y) (+ x y))
        |(define sub (lambda (x y) (- x y)))
      """.stripMargin
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }

  "Local variable inside function" should "be enabled" in {
    val code =
      """
        |(define (f x y)
        |  (define z (+ x y))
        |  z)
        |// this is a comment
        |(define f (lambda (x y)
        |  (define z (+ x y))
        |  z))
      """.stripMargin
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }


  "Closures" should "be parsed" in {
    val code =
      """
        |(define (add x y)
        |  (+ x y))
        |
        |(define (add1 y)
        |  (add 1 y))
      """.stripMargin
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }

  "Recursion" should "be parsed" in {
    val code =
      """
        |(define (factorial n)
        |  (if (== n 1)
        |      1
        |      (* n (factorial (- n 1)))))
        |
        |(define (fib n)
        |  (if (|| (== n 1) (== n 2))
        |      1
        |      (fib (- n 1))))
      """.stripMargin
    val maybeAst = parser.parse(code)
    println(maybeAst)
    assert(maybeAst.isRight)
  }
}
