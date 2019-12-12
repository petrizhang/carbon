package com.pzque.coco.typer2

sealed trait Kind {
  /**
    * A right associative operator to construct a [[KFun]] instance.
    * a ~>: b ~>: c = c.~>:(b).~>:(a) = KFun(a, KFun(b,c))
    *
    * @param from
    * @return
    */
  def ~>:(from: Kind): Kind = KFun(from, this)
}

case object Star extends Kind {
  override lazy val toString: String = "*"
}

case class KFun(from: Kind, to: Kind) extends Kind {
  override lazy val toString: String = {
    if (from.isInstanceOf[KFun]) s"($from) -> $to"
    else s"$from -> $to"
  }
}
