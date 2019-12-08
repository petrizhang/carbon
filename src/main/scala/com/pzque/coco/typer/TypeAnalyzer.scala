package com.pzque.coco.typer

import scala.collection.mutable

// TODO: to support recursive function call
class TypeAnalyzer {
  val reporter = new Reporter

  var count = 0

  def newTypeVariable(): TVar = {
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
        vars.foreach(v => replaceMap.put(v, newTypeVariable()))
        replace(body, replaceMap)
    }
  }

  def gen(context: TypeContext, t: Type): Scheme = {
    val params = t.freeTypeVariables -- context.freeTypeVariables
    if (params.isEmpty) {
      t
    } else {
      PolyType(params.toArray, t)
    }
  }

  def unify(ta: Type, tb: Type): Type = {
    if (ta == tb) return ta
    unifyHelper(ta, tb, mutable.Set.empty)
    val required = ta.instance
    val found = tb.instance
    if (required != found) {
      reporter.error(new TypeMismatch(found, required))
    }
    required
  }

  // TODO: fix here
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
      case (Generic(aName, aParams), Generic(bName, bParams)) =>
        if (aName != bName || aParams.length != bParams.length) {
          reporter.error(new TypeMismatch(ta, tb.instance))
        }
        aParams zip bParams foreach { case (aParamType, bParamType) =>
          unifyHelper(aParamType, bParamType, instantiatedSet)
        }
      case _ =>
        reporter.error(new TypeMismatch(ta.instance, tb.instance))
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
        val resultType = newTypeVariable()
        val a = unify(TFunc(givenArgType, resultType), givenFuncType)
        a.asInstanceOf[TFunc].to

      // Lambda expression
      case Lambda(argName, body) =>
        val argType = newTypeVariable()
        val retType = analyze(context + (argName -> argType), body)
        TFunc(argType, retType).instance

      // Let expression
      case Let(name, definition, body) =>
        val bodyType = analyze(context, definition)
        val genBodyScheme = gen(context, bodyType)
        analyze(context + (name -> genBodyScheme), body)

      // Letrec expression
      case LetRec(name, definition, body) =>
        var tmpDefinitionType: Type = newTypeVariable()
        val newContext = context + (name -> tmpDefinitionType)
        val analyzedDefinitionType = analyze(newContext, definition)
        tmpDefinitionType = tmpDefinitionType.instance
        val bodyType = unify(tmpDefinitionType, analyzedDefinitionType)
        val genBodyScheme = gen(newContext, bodyType)
        analyze(context + (name -> genBodyScheme), body)
    }
  }
}

object HMWRun extends App {
  /**
    * TODO: fix the bug when a type lambda's parameters and body are
    * using different references of a single Type Variable.
    * e.g.
    * TFunc(TVar("a"), Generic("List", TVar("a"))) is a wrong construction.
    * where TVar("a") in parameters and body are two different objects.
    */

  val a = TVar("a")
  val b = TVar("b")

  val prelude = Map(
    "pair" -> TFunc(b, TFunc(a, Generic("Pair", Array(a, b))))
  )

  val e0 = LitBool(true)
  val e1 = LitInt(1)
  val e2 = Lambda("x", Var("x"))
  val e3 = Apply(e2, e1)
  val e4 = Let("x", e0, Var("x"))
  val e5 = Let("f", Lambda("x", Var("x")), Apply(Var("f"), Var("f")))
  val e6 = Lambda("y", Let("f", Lambda("x", Apply(Var("x"), Var("y"))), Var("f")))
  val e7 = LetRec("fix",
    Lambda("f",
      Apply(Var("f"),
        Apply(Var("fix"), Var("f")))),
    Var("fix"))

  val e8 = Apply(Apply(Var("pair"), LitInt(1)), LitInt(2))
  val analyzer = new TypeAnalyzer
  val preContext = new TypeContext(prelude)

  val t = analyzer.analyze(preContext, e8)
  println(t)
}