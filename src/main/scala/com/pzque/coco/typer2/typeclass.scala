package com.pzque.coco.typer2

import com.pzque.coco.typer2.types.Types

import scala.collection.mutable.ArrayBuffer

object typeclass {

  /**
    * The type [[target]] instantiates class `id`.
    * e.g. Pred("Num", tInt) means that tInt instantiates the "Num" class
    *
    * @param id
    * @param target
    */
  case class IsInst(id: String, target: Type) {
    override lazy val toString: String = s"$id $target"
  }

  case class Qual[T: Types](context: List[IsInst], pred: T) {
    override lazy val toString: String = {
      s"""${context.mkString("(", ", ", ")")} => $pred"""
    }
  }

  type Inst = Qual[IsInst]

  case class Class(supers: ArrayBuffer[String], instances: ArrayBuffer[Inst]) {
    override def toString: String = {
      s"""${supers.mkString("[", ",", "]")}
         |${instances.mkString("[", ",\n", "]")}
         |""".stripMargin
    }
  }

}
