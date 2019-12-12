package com.pzque.coco.typer2

import com.pzque.coco.typer2.types.{Types, predImplTypes}
import com.pzque.coco.typer2.unify.matchingPred

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import implicits._

object typeclass {

  type Inst = Qual[Pred]

  case class Class(supers: ArrayBuffer[String], instances: ArrayBuffer[Inst]) {
    override def toString: String = {
      s"""${supers.mkString("[", ",", "]")}
         |${instances.mkString("[", ",\n", "]")}
         |""".stripMargin
    }
  }

  /**
    * The type [[target]] instantiates class `id`.
    * e.g. Pred("Num", tInt) means that tInt instantiates the "Num" class
    *
    * @param id
    * @param target
    */
  case class Pred(id: String, target: Type) {
    override lazy val toString: String = s"$id $target"
  }

  case class Qual[T: Types](bases: List[Pred], pred: T) {
    override lazy val toString: String = {
      s"""${bases.mkString("(", ", ", ")")} => $pred"""
    }
  }

  /**
    * Infer valid predicates by super class relation.
    *
    * e.g. "a" is a instance of class "C",
    * *    then a is a instance of all supper classes of "C".
    *
    * @param classEnv
    * @param pred
    * @return
    */
  final def bySuper(classEnv: ClassEnv, pred: Pred): ArrayBuffer[Pred] = {
    val id = pred.id
    val target = pred.target
    classEnv.supers(id) match {
      case Some(suppers) =>
        suppers.flatMap(supperClass => bySuper(classEnv, supperClass $ target)).addOne(pred)
      case None => ArrayBuffer()
    }
  }

  /**
    * Infer valid predicates by instance relations.
    *
    * e.g. `pred` = `h` in `ps :=> h`
    * *    and applySubst(`pred`, `h`) = s
    * *    for every x in `ps`, applySubst(s, x) is true
    *
    * @param classEnv
    * @param pred
    * @return
    */
  final def byInst(classEnv: ClassEnv, pred: Pred): Option[List[Pred]] = {
    val id = pred.id
    classEnv.instances(id) match {
      case Some(instances) =>
        val inferredPredicates = instances map { inst =>
          tryInst(inst, pred)
        }
        val i = inferredPredicates.indexWhere(_.isSuccess)
        if (i == -1) None else Some(inferredPredicates(i).get)
      case None => None
    }
  }

  /**
    * If `pred` = `h` in `ps :=> h` ( i.e. Qual(ps, h) ),
    * we could get that every predicate x in `ps` must be true(after x being substituted).
    *
    * @param inst
    * @param pred
    * @return
    */
  final def tryInst(inst: Inst, pred: Pred): Try[List[Pred]] = {
    val ps = inst.bases
    val h = inst.pred
    for (subst <- matchingPred(h, pred))
      yield ps.map(x => predImplTypes.applySubst(subst, x))
  }

  /**
    * Check if predicates `ps` can derive predicate `p`
    *
    * @param ce
    * @param ps
    * @param p
    * @return
    */
  final def entail(ce: ClassEnv, ps: List[Pred], p: Pred): Boolean = {
    ps.map(bySuper(ce, _)).contains(p) ||
      (byInst(ce, p) match {
        case None => false
        case Some(qs) =>
          qs.forall(entail(ce, ps, _))
      })
  }
}
