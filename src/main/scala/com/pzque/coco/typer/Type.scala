package com.pzque.coco.typer

import scala.collection.mutable

sealed trait Scheme {
  val freeTypeVariables: Set[String]
}

sealed trait Type extends Scheme {
  def instance: Type
}

case object TBool extends Type {
  override def instance: Type = TBool

  lazy val freeTypeVariables: Set[String] = Set.empty

  override def toString: String = "Boolean"
}

case object TInt extends Type {
  override def instance: Type = TInt

  lazy val freeTypeVariables: Set[String] = Set.empty

  override def toString: String = "Int"
}

case class TVar(name: String) extends Type {
  private var _instance: Type = this

  override def instance: Type = _instance match {
    case tv: TVar => if (tv == this) this else tv.instance
    case _ => _instance.instance
  }

  def instance_=(value: Type): Unit = {
    _instance = value
  }

  lazy val freeTypeVariables: Set[String] = Set(name)

  override def toString: String = s"$name"
}

case class TFunc(from: Type, to: Type) extends Type {
  override def instance: Type = TFunc(from.instance, to.instance)

  lazy val freeTypeVariables: Set[String] = from.freeTypeVariables ++ to.freeTypeVariables

  override def toString: String = {
    val fromString = if (from.isInstanceOf[TFunc]) s"($from)" else from.toString
    s"$fromString -> $to"
  }
}

case class Generic(name: String, params: Array[Type]) extends Type {
  override def instance: Type = Generic(name, params.map(_.instance))

  override lazy val freeTypeVariables: Set[String] = params.flatMap(_.freeTypeVariables).toSet

  override def toString: String = s"""$name ${params.mkString(" ")}"""

  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[Generic] && obj.toString == toString
  }
}

/**
  * This class is used to achieve let polymorphism.
  * e.g.
  * let f = \x => x in f f
  * *   |            --| |--
  * *  1st          2nd  3rd
  * There 3 'f' occurs in above let expression, we first generalize the 1st 'f' as a 'PolyType'
  * then the 2nd and 3rd 'f' will be instantiated with two different types.
  * Thus the type inference procedure could continue.
  *
  * Here `f f` means apply a function of type `T1 -> T1` to a parameter of type `T2 -> T2`.
  * Rather than apply a `T -> T` function to a `T -> T` type, which is obviously an error.
  *
  * @param vars
  * @param body
  */
case class PolyType(vars: Array[String], body: Type) extends Scheme {
  lazy val freeTypeVariables: Set[String] = body.freeTypeVariables -- vars

  override def toString: String = vars.mkString("$", ".$", s" => ${body.toString}")
}

