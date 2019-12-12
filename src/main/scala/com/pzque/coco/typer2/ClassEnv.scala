package com.pzque.coco.typer2

import typeclass._
import unify.mguPred

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ClassEnv {
  private val _classes: mutable.Map[String, Class] = mutable.Map.empty
  private val _defaults: ArrayBuffer[Type] = ArrayBuffer.empty

  def getClass(id: String): Option[Class] = _classes.get(id)

  def defaults: ArrayBuffer[Type] = _defaults

  def defined(id: String): Boolean = _classes.contains(id)

  def supers(id: String): Option[ArrayBuffer[String]] = {
    _classes.get(id).map(x => x.supers)
  }

  def instances(id: String): Option[ArrayBuffer[Inst]] = {
    _classes.get(id).map(x => x.instances)
  }

  def addClass(id: String, suppers: ArrayBuffer[String]): ClassEnv = {
    if (defined(id))
      throw new Error(s"class already defined: $id")
    if (suppers.exists(x => !defined(x)))
      throw new Error("supper class not defined")
    _classes.put(
      id,
      Class(suppers, ArrayBuffer())
    )
    this
  }

  def addInstance(inst: Inst): ClassEnv = {
    val pred = inst.pred
    val id = pred.id

    if (!defined(id))
      throw new Error("no class for instance")

    val clazz = _classes(id)
    val clazzInstances = clazz.instances

    if (clazzInstances.exists(ins => overlap(ins.pred, pred)))
      throw new Error("overlapping instance")

    clazz.instances += inst
    this
  }

  final def overlap(p1: Pred, p2: Pred): Boolean = {
    try {
      mguPred(p1, p2)
      true
    } catch {
      case _: Error => false
    }
  }
}
