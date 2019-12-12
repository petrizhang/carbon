package com.pzque.coco.typer2

import typeclass.Pred
import substitution._
import types._
import implicits._

object unify {

  /**
    * Most general unification.
    * Find a substitution s that satisfies `applySubst s actual = applySubst s expected`.
    *
    * @param actual
    * @param expected
    * @return
    */
  final def mgu(actual: Type, expected: Type): Subst = {
    (actual, expected) match {
      case (TAp(f1, arg1), TAp(f2, arg2)) =>
        val s1 = mgu(f1, f2)
        val s2 = mgu(
          typeImplTypes.applySubst(s1, arg1),
          typeImplTypes.applySubst(s1, arg2)
        )
        s2 @@ s1
      case (tv: TVar, t) => varBind(tv, t)
      case (t, tv: TVar) => varBind(tv, t)
      case (u: TCon, t: TCon) if t == u => nullSubst
      case _ => throw new Error("types do not unify")
    }
  }

  final def varBind(u: TVar, t: Type): Subst = {
    if (u == t) {
      nullSubst
    } else if (typeImplTypes.typeVariables(t).contains(u.id)) {
      throw new Error("occurs check fails")
    } else if (u.kind != t.kind) {
      throw new Error("kinds do not match")
    } else {
      u +-> t
    }
  }

  /**
    * Match the given type with a expected type,
    * i.e. find a substitution s that satisfies `applySubst s actual = expected`.
    *
    * @param actual
    * @param expected
    * @return
    **/
  final def matching(actual: Type, expected: Type): Subst = {
    (actual, expected) match {
      case (TAp(f1, arg1), TAp(f2, arg2)) =>
        val s1 = matching(f1, f2)
        val s2 = matching(arg1, arg2)
        merge(s1, s2)
      case (u: TVar, t) if u.kind == t.kind =>
        u +-> t
      case (tc1: TCon, tc2: TCon) if tc1 == tc2 =>
        nullSubst
      case _ => throw new Error("types do not match")
    }
  }

  /*
  mguPred, matchPred :: Pred -> Pred -> Maybe Subst
  mguPred             = lift mgu
  matchPred           = lift match
  
  lift m (IsIn i t) (IsIn i' t')
           | i == i'   = m t t'
           | otherwise = fail "classes differ"
   */

  final def mguPred(actual: Pred, expected: Pred): Subst = {
    if (actual.id != expected.id) throw new Error("classes differ")
    mgu(actual.body, expected.body)
  }

  final def matchingPred(actual: Pred, expected: Pred): Subst = {
    if (actual.id != expected.id) throw new Error("classes differ")
    matching(actual.body, expected.body)
  }
}
