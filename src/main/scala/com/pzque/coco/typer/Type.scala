package com.pzque.coco.typer

import scala.collection.mutable

sealed trait Scheme {
  val freeTypeVariables: Set[String]
}

sealed trait Type extends Scheme

case class TypeVariable(name: String) extends Type {
  lazy val freeTypeVariables: Set[String] = Set(name)

  override def toString: String = s"$name"
}

// TODO: check currying situations carefully
// e.g. TFunc(["a","b"], ["c","d"] -> "f") should be ["a","b","c","d"] -> "f"
case class FunctionType(from: Array[Type], to: Type) extends Type {
  lazy val freeTypeVariables: Set[String] = from.flatMap(_.freeTypeVariables).toSet ++ to.freeTypeVariables

  def partialApply(n: Int): FunctionType = {
    val heads = from.view.slice(0, n)
    val tail = from.view.slice(n, from.length)

    val newTo = to match {
      case FunctionType(fargs, fto) =>
        FunctionType((tail ++ fargs).toArray, fto)
      case _ => FunctionType(tail.toArray, to)
    }
    FunctionType(heads.toArray[Type], newTo)
  }

  override def toString: String = {
    val fromString = FunctionType.formatArgTypes(from)
    s"$fromString$to"
  }

  // TODO: optimize here
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[FunctionType] && obj.toString == toString
  }
}

object FunctionType {
  def formatArgTypes(argTypes: Array[Type]): String = {
    val builder = new mutable.StringBuilder
    argTypes foreach { t =>
      val repr = if (t.isInstanceOf[FunctionType]) s"($t) -> " else s"$t -> "
      builder.append(repr)
    }
    builder.toString()
  }
}

/**
  * Higher-order type operator.
  * e.g.
  * Pair a b
  * Cons a (List a)
  *
  * @param name   kind name
  * @param params type parameters
  */
case class Kind(name: String, params: Array[Type]) extends Type {
  override lazy val freeTypeVariables: Set[String] = params.flatMap(_.freeTypeVariables).toSet

  override def toString: String = {
    if (params.isEmpty) name else s"""$name ${params.mkString(" ")}"""
  }

  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[Kind] && obj.toString == toString
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
  * @param vars type parameters(name of type variables)
  * @param body actual type
  */
case class PolyType(vars: Array[String], body: Type) extends Scheme {
  lazy val freeTypeVariables: Set[String] = body.freeTypeVariables -- vars

  override def toString: String = vars.mkString("$", ".$", s" => ${body.toString}")
}

object TypeSystem {
  val TBoolean: Kind = Kind("Boolean", Array())
  val TInt: Kind = Kind("Int", Array())
  val TUnit: Kind = Kind("Unit", Array())
}