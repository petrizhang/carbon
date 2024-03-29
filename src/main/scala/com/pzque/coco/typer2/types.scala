package com.pzque.coco.typer2

import substitution._
import typeclass._
import implicits._

object types {

  trait Types[T] {
    def applySubst(subst: Subst, t: T): T

    def typeVariables(t: T): Set[String]
  }

  implicit val typeImplTypes: Types[Type] = new Types[Type] {
    override def applySubst(subst: Subst, t: Type): Type = {
      t match {
        case tv: TVar => subst.getOrElse(tv, t)
        case TAp(f, arg) => TAp(
          applySubst(subst, f),
          applySubst(subst, arg)
        )
        case _ => t
      }
    }

    override def typeVariables(t: Type): Set[String] = t.typeVariables
  }

  implicit def listImplTypes[T: Types]: Types[List[T]] = new Types[List[T]] {
    final def applySubst(subst: Subst, t: List[T]): List[T] = {
      val impl = implicitly[Types[T]]
      t.map(x => impl.applySubst(subst, x))
    }

    final def typeVariables(t: List[T]): Set[String] = {
      val impl = implicitly[Types[T]]
      t.flatMap(x => impl.typeVariables(x)).toSet
    }
  }

  implicit val predImplTypes: Types[Pred] = new Types[Pred] {
    override def applySubst(subst: Subst, pred: Pred): Pred = {
      Pred(pred.id, typeImplTypes.applySubst(subst, pred.target))
    }

    override def typeVariables(t: Pred): Set[String] = typeImplTypes.typeVariables(t.target)
  }

  implicit def qualImplTypes[T: Types]: Types[Qual[T]] = new Types[Qual[T]] {
    override def applySubst(subst: Subst, qual: Qual[T]): Qual[T] = {
      val tImpl = implicitly[Types[T]]
      listImplTypes[Pred].applySubst(subst, qual.bases) :=> tImpl.applySubst(subst, qual.pred)
    }

    override def typeVariables(t: Qual[T]): Set[String] = {
      val impl = implicitly[Types[T]]
      listImplTypes[Pred].typeVariables(t.bases) ++ impl.typeVariables(t.pred)
    }
  }
}