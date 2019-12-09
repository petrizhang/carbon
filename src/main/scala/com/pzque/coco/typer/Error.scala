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

class TypeMismatchError(found: Type, required: Type) extends AbsError(
  s"""
     | type mismatch:
     | required: $required
     | found : $found
     |""".stripMargin
)


class TypeMismatchDueToOccursCheck(occursCheckMsg: String,
                                   found: Array[Type],
                                   required: Array[Type]) extends AbsError(
  s"""
     | $occursCheckMsg
     | required:  ${required.mkString(",")}
     | found : ${found.mkString(",")}
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

class TooManyArguments(required: Int, found: Int) extends AbsError(
  s"""
     | too many arguments:
     | required:  ${required}
     | found : ${found}
     |""".stripMargin
)

class OccursCheckError(left: Type, right: Type) extends AbsError(
  s"""occurs check: cannot construct the infinite type: $left ~ $right"""
)

class Reporter {
  def error(err: AbsError): Nothing = throw err
}
