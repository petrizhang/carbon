package com.pzque.coco.cps

sealed trait Cont;

case class Add1Cont(right: AST, cont: Cont) extends Cont

case class Add2Cont(left_result: AnyRef, cont: Cont) extends Cont

case class EndCont() extends Cont

sealed trait AST

case class Literal(value: java.lang.Double) extends AST

case class Add(left: AST, right: AST) extends AST

object Test {
  var cont: Cont = EndCont();
  var env: Map[String, AnyRef] = Map.empty
  var expr: AST = null
  var result: AnyRef = null;

  def ApplyCont(): Unit = {
    cont match {
      case _: EndCont =>
        println(s"End of computation: ${result}")
      case Add1Cont(right, saved_cont) =>
        cont = Add2Cont(result, saved_cont)
        expr = right
        Eval()
      case Add2Cont(left_result, saved_cont) =>
        result = (left_result.asInstanceOf[java.lang.Double]
          + result.asInstanceOf[java.lang.Double]).asInstanceOf[AnyRef]
        cont = saved_cont
        ApplyCont()
    }
  }

  def Eval(): Unit = {
    expr match {
      case Literal(value) =>
        result = value
        ApplyCont()
      case Add(left, right) =>
        cont = Add1Cont(right, cont)
        expr = left
        Eval()
    }
  }

  def Run(expr: AST): Unit = {
    this.expr = expr
    this.cont = EndCont()
    Eval()
  }
}

object App {
  def main(args: Array[String]): Unit = {
    val left = Add(Literal(1.0), Literal(2.0))
    val right = Add(Literal(3.0), Literal(4.0))
    val ast = Add(left, right)
    Test.Run(ast)
  }
}
