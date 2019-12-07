package com.pzque.coco.typer


class TypeContext(val internalMap: Map[String, Scheme], val freeTypeVariables: Set[String]) {

  def this(internalMap: Map[String, Scheme]) {
    this(internalMap, internalMap.values.foldLeft(Set.empty[String]) {
      case (left, right) => left ++ right.freeTypeVariables
    })
  }

  def +(p: (String, Scheme)): TypeContext = {
    new TypeContext(internalMap + p, freeTypeVariables ++ p._2.freeTypeVariables)
  }

  def get(name: String): Option[Scheme] = internalMap.get(name)

  def getOrElse[T >: Scheme](name: String, default: T): T = internalMap.getOrElse(name, default)

}

object TypeContext {
  def empty = new TypeContext(Map.empty, Set.empty)
}
