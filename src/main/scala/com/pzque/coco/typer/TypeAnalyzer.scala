package com.pzque.coco.typer

import scala.collection.mutable

// TODO: to support recursive function call
object TypeAnalyzer {
  val reporter = new Reporter

  var count = 0

  def makeFreshTVar(): TVar = {
    count += 1
    TVar(s"'t$count")
  }

  final def replace(t: Type, according: mutable.Map[String, TVar]): Type = {
    t match {
      case TBool => TBool
      case TInt => TInt
      case TVar(name) =>
        according.getOrElse(name, TVar(name))
      case TFunc(from, to) =>
        TFunc(replace(from, according), replace(to, according))
    }
  }

  // Instance a polytype (apply a type lambda)
  def inst(context: TypeContext, scheme: Scheme): Type = {
    scheme match {
      case t: Type => t
      case PolyType(vars, body) =>
        assert(body.isInstanceOf[Type], "expected a monotype but got a polytype")
        val replaceMap = mutable.Map.empty[String, TVar]
        vars.foreach(v => replaceMap.put(v, makeFreshTVar()))
        replace(body, replaceMap)
    }
  }

  def gen(context: TypeContext, t: Type): Scheme = {
    val params = t.freeTypeVariables -- context.freeTypeVariables
    if (params.isEmpty) {
      t
    } else {
      PolyType(params.toList, t)
    }
  }

  def unify(ta: Type, tb: Type): Type = {
    if (ta == tb) return ta
    unifyHelper(ta, tb, mutable.Set.empty)
    ta.instance
  }

  def unifyHelper(ta: Type, tb: Type, instantiatedSet: mutable.Set[String]): Unit = {
    (ta, tb) match {
      case (av: TVar, bv: TVar) =>
        if (instantiatedSet.contains(av.name)) {
          bv.instance = av.instance
          instantiatedSet.add(bv.name)
        } else if (instantiatedSet.contains(bv.name)) {
          av.instance = bv.instance
          instantiatedSet.add(av.name)
        } else {
          av.instance = bv
          instantiatedSet.add(av.name)
        }
      case (av: TVar, _) =>
        av.instance = tb
        instantiatedSet.add(av.name)
      case (_, bv: TVar) =>
        bv.instance = ta
        instantiatedSet.add(bv.name)
      case (TFunc(aFrom, aTo), TFunc(bFrom, bTo)) =>
        unifyHelper(aFrom, bFrom, instantiatedSet)
        unifyHelper(aTo, bTo, instantiatedSet)
      case _ => assert(ta == tb)
    }
  }

  def analyze(context: TypeContext, expr: Expr): Type = {
    expr match {
      // Boolean literal
      case LitBool(v) => TBool

      // Int literal
      case LitInt(v) => TInt

      // Variable
      case Var(name) =>
        context.get(name) match {
          case Some(t) => inst(context, t)
          case _ => sys.error(f"unresolved variable: $name")
        }

      // Function application
      case Apply(func, arg) =>
        val givenFuncType = analyze(context, func)
        val givenArgType = analyze(context, arg)
        val resultType = makeFreshTVar()
        val a = unify(TFunc(givenArgType, resultType), givenFuncType)
        a.asInstanceOf[TFunc].to

      // Lambda expression
      case Lambda(argName, body) =>
        val argType = makeFreshTVar()
        val retType = analyze(context + (argName -> argType), body)
        TFunc(argType, retType).instance

      // Let expression
      case Let(name, body, ret) =>
        val bodyType = analyze(context, body)
        val genBodyScheme = gen(context, bodyType)
        analyze(context + (name -> genBodyScheme), ret)
    }
  }
}

object HMWRun extends App {
  val e0 = LitBool(true)
  val e1 = LitInt(1)
  val e2 = Lambda("x", Var("x"))
  val e3 = Apply(e2, e1)
  val e4 = Let("x", e0, Var("x"))
  val e5 = Let("f", Lambda("x", Var("x")), Apply(Var("f"), Var("f")))
  val e6 = Lambda("y", Let("f", Lambda("x", Apply(Var("x"), Var("y"))), Var("f")))

  val t = TypeAnalyzer.analyze(TypeContext.empty, Lambda("y", Apply(Var("y"), LitInt(1))))
  println(t)
}