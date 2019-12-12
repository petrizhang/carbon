package com.pzque.coco.typer2

import substitution._
import typeclass._

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

    override def typeVariables(t: Type): Set[String] = {
      t match {
        case TVar(id, _) => Set(id)
        case TAp(f, args) => typeVariables(f) ++ typeVariables(args)
        case _ => Set.empty[String]
      }
    }
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
      Pred(pred.id, typeImplTypes.applySubst(subst, pred.body))
    }

    override def typeVariables(t: Pred): Set[String] = typeImplTypes.typeVariables(t.body)
  }

  implicit def qualImplTypes[T: Types]: Types[Qual[T]] = new Types[Qual[T]] {
    override def applySubst(subst: Subst, qual: Qual[T]): Qual[T] = {
      val tImpl = implicitly[Types[T]]
      listImplTypes[Pred].applySubst(subst, qual.context) :=> tImpl.applySubst(subst, qual.body)
    }

    override def typeVariables(t: Qual[T]): Set[String] = {
      val impl = implicitly[Types[T]]
      listImplTypes[Pred].typeVariables(t.context) ++ impl.typeVariables(t.body)
    }
  }
}