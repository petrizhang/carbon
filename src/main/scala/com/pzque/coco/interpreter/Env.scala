package com.pzque.coco.interpreter

import scala.collection.mutable


class Env(val parent: Option[Env] = None) {
  private var vars: mutable.Map[String, Any] = mutable.Map[String, Any]()

  def newVar(name: String, value: Any): Unit = {
    vars += (name -> value)
  }

  def resolve(name: String): Option[Any] = {
    val value = vars.get(name)
    if (value.nonEmpty) {
      value
    } else {
      parent.flatMap(env => env.resolve(name))
    }
  }

  def newChild: Env = new Env(Some(this))
}
