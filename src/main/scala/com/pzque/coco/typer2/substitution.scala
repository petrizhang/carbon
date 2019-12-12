package com.pzque.coco.typer2

import types._

import scala.util.Try

object substitution {
  type Subst = Map[TVar, Type]

  val nullSubst: Subst = Map.empty[TVar, Type]

  /*
    merge      :: Monad m => Subst -> Subst -> m Subst
    merge s1 s2 = if agree then return (s1++s2) else fail "merge fails"
    where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                      (map fst s1 `intersect` map fst s2)
   */
  final def merge(s1: Subst, s2: Subst): Try[Subst] = Try {
    if (agree(s1, s2)) s1 ++ s1
    else throw new Error("merge fails")
  }

  final def agree(s1: Subst, s2: Subst): Boolean = {
    val intersect = s1.keySet.intersect(s2.keySet)
    intersect.iterator.forall(v =>
      typeImplTypes.applySubst(s1, v) == typeImplTypes.applySubst(s2, v)
    )
  }
}
