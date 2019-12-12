package com.pzque.coco.typer2

import com.pzque.coco.typer2.types.Types

object typeclass {

  implicit class StringPred(id: String) {
    def $(t: Type): Pred = Pred(id, t)
  }

  implicit class PredListToQual(list: List[Pred]) {
    def :=>[T: Types](body: T): Qual[T] = Qual(list, body)
  }

  case class Pred(id: String, body: Type) {
    override lazy val toString: String = s"$id $body"
  }

  case class Qual[T: Types](context: List[Pred], body: T) {
    override lazy val toString: String = {
      s"""${context.mkString("(", ", ", ")")} => $body"""
    }
  }

  type Inst = Qual[Pred]
  type Class = (List[String], List[Inst])

}
