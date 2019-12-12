package com.pzque.coco.typer2

import typeclass.Pred
import substitution._
import types._
import implicits._

import scala.util.{Failure, Success, Try}

object unify {

  /**
    * Most general unification.
    * Find a substitution s that satisfies `applySubst s actual = applySubst s expected`.
    *
    * @param actual
    * @param expected
    * @return
    */
  final def mgu(actual: Type, expected: Type): Try[Subst] = {
    (actual, expected) match {
      case (TAp(f1, arg1), TAp(f2, arg2)) =>
        for (s1 <- mgu(f1, f2);
             s2 <- mgu(
               typeImplTypes.applySubst(s1, arg1),
               typeImplTypes.applySubst(s1, arg2)
             )) yield s2 @@ s1
      case (tv: TVar, t) => varBind(tv, t)
      case (t, tv: TVar) => varBind(tv, t)
      case (u: TCon, t: TCon) if t == u => Success(nullSubst)
      case _ => Failure(new Error("types do not unify"))
    }
  }

  final def varBind(u: TVar, t: Type): Try[Subst] = {
    if (u == t) {
      Success(nullSubst)
    } else if (typeImplTypes.typeVariables(t).contains(u.id)) {
      Failure(new Error("occurs check fails"))
    } else if (u.kind != t.kind) {
      Failure(new Error("kinds do not match"))
    } else {
      Success(u +-> t)
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
  final def matching(actual: Type, expected: Type): Try[Subst] = {
    (actual, expected) match {
      case (TAp(f1, arg1), TAp(f2, arg2)) =>
        for (s1 <- matching(f1, f2);
             s2 <- matching(arg1, arg2);
             ret <- merge(s1, s2))
          yield ret
      case (u: TVar, t) if u.kind == t.kind =>
        Success(u +-> t)
      case (tc1: TCon, tc2: TCon) if tc1 == tc2 =>
        Success(nullSubst)
      case _ => Failure(new Error("types do not match"))
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

  final def mguPred(actual: Pred, expected: Pred): Try[Subst] = {
    if (actual.id != expected.id) Failure(new Error("classes differ"))
    mgu(actual.target, expected.target)
  }

  final def matchingPred(actual: Pred, expected: Pred): Try[Subst] = {
    if (actual.id != expected.id) Failure(new Error("classes differ"))
    matching(actual.target, expected.target)
  }
}
