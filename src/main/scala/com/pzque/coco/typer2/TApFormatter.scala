package com.pzque.coco.typer2

trait TApFormatter {
  def format(ap: TAp): String
}

object formatters {
  val funcFormatter: TApFormatter = {
    case TAp(TAp(_, from: TAp), to) if from.isFunction => s"($from) -> $to"
    case TAp(TAp(_, from), to) => s"$from -> $to"
  }

  val normalFormatter: TApFormatter = (ap: TAp) => s"${ap.f} ${ap.arg}"

  val listFormatter: TApFormatter = (ap: TAp) => s"[ap.arg.toString]"

  val pairFormatter: TApFormatter = {
    case TAp(TAp(_, from), to) => s"($from, $to)"
  }
}