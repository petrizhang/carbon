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
     | Type mismatch:
     | found : $found
     | required: $required
     |""".stripMargin
)


class TypeMismatchDueToOccursCheck(occursCheckMsg: String, found: Array[Type], required: Array[Type]) extends AbsError(
  s"""
     | $occursCheckMsg
     | found : ${found.mkString(",")}
     | required:  ${required.mkString(",")}
     |""".stripMargin
)

class OccursCheckError(left: Type, right: Type) extends AbsError(
  s"""Occurs check: cannot construct the infinite type: $left ~ $right"""
)

class Reporter {
  def error(err: AbsError): Nothing = throw err
}
