package com.pzque.coco.typer

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

  override def instance: Type = _instance

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

case class PolyType(vars: List[String], body: Type) extends Scheme {
  lazy val freeTypeVariables: Set[String] = body.freeTypeVariables -- vars

  override def toString: String = vars.mkString("$", ".$", s" => ${body.toString}")
}

