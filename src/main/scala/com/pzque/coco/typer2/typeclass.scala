package com.pzque.coco.typer2

import com.pzque.coco.typer2.types.Types

import scala.collection.mutable.ArrayBuffer

object typeclass {

  case class Pred(id: String, body: Type) {
    override lazy val toString: String = s"$id $body"
  }

  case class Qual[T: Types](context: List[Pred], pred: T) {
    override lazy val toString: String = {
      s"""${context.mkString("(", ", ", ")")} => $pred"""
    }
  }

  type Inst = Qual[Pred]

  case class Class(supers: ArrayBuffer[String], instances: ArrayBuffer[Inst]) {
    override def toString: String = {
      s"""${supers.mkString("[", ",", "]")}
         |${instances.mkString("[", ",\n", "]")}
         |""".stripMargin
    }
  }

}
