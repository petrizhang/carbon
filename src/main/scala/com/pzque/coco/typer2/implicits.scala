package com.pzque.coco.typer2

import com.pzque.coco.typer2.substitution.Subst
import com.pzque.coco.typer2.typeclass.{Pred, Qual}
import com.pzque.coco.typer2.types.{Types, typeImplTypes}

import scala.collection.mutable.ArrayBuffer

object implicits {
  def ab[T]: ArrayBuffer[T] = ArrayBuffer.empty[T]

  def ab[T](args: T*): ArrayBuffer[T] = ArrayBuffer(args: _*)

  implicit def stringToTVar(id: String): Type = TVar(id)

  implicit class SubstCompose(s1: Subst) {
    // applySubst (s1 @@ s2) = applySubst s1 . applySubst s2
    def @@(s2: Subst): Subst = {
      val ret = s2.map { case (key, value) =>
        (key, typeImplTypes.applySubst(s1, value))
      }
      s1 ++ ret
    }
  }

  implicit class StringPred(id: String) {
    def $(t: Type): Pred = Pred(id, t)
  }

  implicit class PredListToQual(list: List[Pred]) {
    def :=>[T: Types](body: T): Qual[T] = Qual(list, body)
  }

}
