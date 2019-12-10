package com.pzque.coco.typer

sealed abstract class AbsError(val message: String)
  extends Exception(message) {

  def this(message: String, cause: Throwable) {
    this(message)
    initCause(cause)
  }

  def this(cause: Throwable) {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() {
    this(null: String)
  }
}

class TypeMismatchError(expected: Type, actual: Type) extends AbsError(
  s"""
     | type mismatch:
     | expected type: $expected
     |   actual type: $actual
     |""".stripMargin
)

class TypeMismatchDueToOccursCheck(occursCheckMsg: String,
                                   expected: Type,
                                   actual: Type) extends AbsError(
  s"""
     | $occursCheckMsg
     | expected type: ${expected}
     |   actual type: ${actual}
     |""".stripMargin
)

class CannotFindVariable(name: String) extends AbsError(
  s"""
     | cannot find variable `$name`
     |""".stripMargin
)

class IsNotAFunction(expr: Expr) extends AbsError(
  s"""
     | `$expr` is not a function
     |""".stripMargin
)

class TooManyArguments(expected: Int, actual: Int) extends AbsError(
  s"""
     | too many arguments:
     | expected:  ${expected}
     |   actual: ${actual}
     |""".stripMargin
)

class OccursCheckError(name: String, right: Type) extends AbsError(
  s"""occurs check: cannot construct the infinite type: $name ~ $right"""
)

class Reporter {
  def error(err: AbsError): Nothing = throw err
}
