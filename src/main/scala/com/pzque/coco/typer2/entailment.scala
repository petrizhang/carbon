package com.pzque.coco.typer2

import typeclass._
import implicits._
import unify._
import types._

import scala.collection.mutable.ArrayBuffer

object entailment {

  /**
    * Find the predicates a type satisfies by super class relation.
    *
    * e.g. "a" is a instance of class "C",
    * *    then a is a instance of all supper classes of "C".
    *
    * @param classEnv
    * @param pred
    * @return
    */
  final def bySuper(classEnv: ClassEnv, pred: IsInst): ArrayBuffer[IsInst] = {
    val id = pred.id
    val target = pred.target
    classEnv.supers(id) match {
      case Some(suppers) =>
        suppers.flatMap(supperClass => bySuper(classEnv, supperClass $ target)).addOne(pred)
      case None => ArrayBuffer()
    }
  }

  /**
    * Find the predicates a type satisfies by instance relation.
    *
    * e.g. "a" is a instance of class "C",
    * *    then a is a instance of all supper classes of "C".
    *
    * @param classEnv
    * @param pred
    * @return
    */
//  final def byInst(classEnv: ClassEnv, pred: IsInst): Option[ArrayBuffer[IsInst]] = {
//    val id = pred.id
//    val target = pred.target
//    classEnv.instances(id) match {
//      case Some(instances) =>
//        instances.foreach { case Qual(ps, h) =>
//          try {
//            val a: Either[Int, String] = Left(1)
//            val b: Either[Int, String] = Right("a")
//            val subst = matchingPred(h, pred)
//          }
//          ps.map(x => predImplTypes.applySubst(subst, x))
//        }
//      case None => None
//    }
//  }
}
