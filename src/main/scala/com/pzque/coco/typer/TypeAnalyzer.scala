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

  def replace(t: Type, according: mutable.Map[String, TVar]): Type = {
    t match {
      case TBool => TBool
      case TInt => TInt
      case TVar(name) =>
        according.getOrElse(name, TVar(name))
      case TFunc(from, to) =>
        TFunc(replace(from, according), replace(to, according))
    }
  }

  /**
    * Instance a polytype (apply a type lambda)
    *
    * @param context
    * @param scheme
    * @return
    */
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

  /**
    * Generalize a monomorphism type to a polymorphism type
    *
    * @param context
    * @param t
    * @return
    */
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
      reporter.error(new TypeMismatchError(found, required))
    }
    required
  }

  // TODO: fix here
  def unifyHelper(ta: Type, tb: Type, instantiatedSet: mutable.Set[String]): Unit = {
    // The [[Generic.equals]] method costs too much, thus we skip generic types
    if (!ta.isInstanceOf[Generic] && !tb.isInstanceOf[Generic] && ta == tb) return

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
          reporter.error(new TypeMismatchError(ta, tb.instance))
        }
        aParams zip bParams foreach { case (aParamType, bParamType) =>
          unifyHelper(aParamType, bParamType, instantiatedSet)
        }
      case _ =>
        reporter.error(new TypeMismatchError(ta.instance, tb.instance))
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
      case If(condition, body, elseBody) =>
        val conditionType = analyze(context, condition)
        unify(conditionType, TBool)
        val bodyType = analyze(context, body)
        val elseType = analyze(context, elseBody)
        unify(bodyType, elseType)

      // Function application
      case Apply(func, arg) =>
        val givenFuncType = analyze(context, func)
        val givenArgType = analyze(context, arg)
        val resultType = newTypeVariable()
        try {
          val a = unify(TFunc(givenArgType, resultType), givenFuncType)
          a.asInstanceOf[TFunc].to
        } catch {
          case e: OccursCheckError =>
            throw new TypeMismatchDueToOccursCheck(e.message,
              givenArgType,
              givenFuncType.asInstanceOf[TFunc].from)
        }
      // Lambda expression
      case Lambda(argName, body) =>
        val argType = newTypeVariable()
        val retType = analyze(context + (argName -> argType), body)
        TFunc(argType, retType).instance

      // Let expression
      case Let(name, definition, body) =>
        val definitionType = analyze(context, definition)
        val genDefinitionType = gen(context, definitionType)
        analyze(context + (name -> genDefinitionType), body)

      // Letrec expression
      case LetRec(bindings, body) =>
        var bindingAnalyzeContext = context
        // Set a temporary type for each variable occurs in bindings
        val tempTypeVariables: Array[TVar] =
          bindings map { case (name, _) =>
            val tempTVar = newTypeVariable()
            bindingAnalyzeContext += (name -> tempTVar)
            tempTVar
          }

        // Unify each bindings
        val bindingNameAndTypes: Array[(String, Type)] =
          tempTypeVariables zip bindings map { case (tempTVar, (name, bindingExpr)) =>
            val exprType = analyze(bindingAnalyzeContext, bindingExpr)
            val tempType = tempTVar.instance // this line must follow the above line
            (name, unify(tempType, exprType))
          }

        // Generalize the expressions's types
        var bodyAnalyzeContext = context
        bindingNameAndTypes foreach { case (name, bindingExprType) =>
          bodyAnalyzeContext += (name -> gen(context, bindingExprType))
        }
        analyze(bodyAnalyzeContext, body)
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
    "pair" -> TFunc(b, TFunc(a, Generic("Pair", Array(a, b)))),
    "test" -> TFunc(Generic("Pair", Array(TInt, TInt)), TInt)
  )

  val e0 = LitBool(true)
  val e1 = LitInt(1)
  val e2 = Lambda("x", Var("x"))
  val e3 = Apply(e2, e1)
  val e4 = Let("x", e0, Var("x"))
  val e5 = Let("f", Lambda("x", Var("x")), Apply(Var("f"), Var("f")))
  val e6 = Lambda("y", Let("f", Lambda("x", Apply(Var("x"), Var("y"))), Var("f")))
  val e7 = LetRec(
    Array(("fix", Lambda("f",
      Apply(Var("f"),
        Apply(Var("fix"), Var("f")))))),
    Apply(Var("fix"), Var("fix"))
  )

  val e8 = Apply(Apply(Var("pair"), LitInt(1)), LitInt(2))
  val e9 = Apply(Var("test"), LitBool(true))

  val e10 = LetRec(
    Array(
      ("f", Lambda("x", If(LitBool(true), LitInt(1), Apply(Var("g"), LitInt(1))))),
      ("g", Lambda("x", If(LitBool(true), LitInt(1), Apply(Var("f"), LitInt(1)))))
    ),
    Apply(Var("f"), LitInt(1))
  )

  val analyzer = new TypeAnalyzer
  val preContext = new TypeContext(prelude)
  val t = analyzer.analyze(preContext, e10)
  println(t)
}