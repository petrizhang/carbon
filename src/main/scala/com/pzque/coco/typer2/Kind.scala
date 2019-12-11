package com.pzque.coco.typer2

sealed trait Kind {
  // must be used as a ~> (b ~> (c ~> ...)) !!!
  def ~>(to: Kind): Kind = KFun(this, to)
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
