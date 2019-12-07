package com.pzque.coco.typer

import scala.reflect.internal.util.Position

sealed abstract class AbsError extends Error {
  def errPos: Position = ???

  def errMsg: String

  override def toString: String = "[Type error at:" + errPos + "] " + errMsg
}

class TypeMismatch(found: Type, required: Type) extends AbsError {
  override def errMsg: String =
    s"""
       | type mismatch:
       | found : $found
       | required: $required
       |""".stripMargin
}

class Reporter {
  def error(err: AbsError): Nothing = sys.error(err.errMsg)
}
