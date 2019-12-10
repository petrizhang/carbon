package com.pzque.coco.typer

import scala.collection.mutable

class Constraint {
  private val _internalMap: mutable.Map[String, Type] = mutable.Map.empty
  private val _valueSet: mutable.Set[String] = mutable.Set.empty

  def internalMap: mutable.Map[String, Type] = _internalMap

  def valueSet: mutable.Set[String] = _valueSet

  def put(key: String, value: Type): Option[Type] = {
    if (occurs(key, value)) throw new OccursCheckError(key, value)
    value match {
      case tv: TVar => _valueSet.add(tv.name)
      case _ =>
    }
    internalMap.put(key, value)
  }

  def contains(key: String): Boolean = internalMap.contains(key)

  def valueContains(value: String): Boolean = valueSet.contains(value)

  def apply(key: String): Type = internalMap(key)

  def getOrElse(key: String, default: Type): Type = internalMap.getOrElse(key, default)

  private def occurs(name: String, right: Type): Boolean = {
    right match {
      case TBool => false
      case TInt => false
      case tv: TVar => tv.name == name
      case TFunc(from, to) =>
        from.exists(t => occurs(name, t)) || occurs(name, to)
      case Generic(name, params) =>
        params.exists(p => occurs(name, p))
    }
  }

}

// TODO: to support recursive function call
class TypeAnalyzer {
  val reporter = new Reporter

  var count = 0

  def newTypeVariable(): TVar = {
    count += 1
    TVar(s"'t$count")
  }

  def replace(t: Type, according: mutable.Map[String, Type]): Type = {
    t match {
      case TBool => TBool
      case TInt => TInt
      case TVar(name) =>
        according.getOrElse(name, TVar(name))
      case TFunc(from, to) =>
        TFunc(from.map(t => replace(t, according)),
          replace(to, according))
      case Generic(name, params) =>
        val newParams = params.map(x => replace(x, according))
        Generic(name, newParams)
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
        val replaceMap = mutable.Map.empty[String, Type]
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

  def unify(expected: Type, actual: Type, constraints: Constraint): Type = {
    unify2(expected, actual, constraints)
  }

  def unify2(expected: Type,
             actual: Type,
             constraints: Constraint): Type = {
    (expected, actual) match {
      case (ev: TVar, av: TVar) =>
        if (ev.name == av.name) {
          ev
        }
        // {e = x} + (e = a) => { e = x, a = x }
        else if (constraints.contains(ev.name)) {
          val ret = constraints(ev.name)
          constraints.put(av.name, ret)
          ret
        }
        // {a = x} + (e = a) => { a = x, e = x }
        else if (constraints.contains(av.name)) {
          val ret = constraints(av.name)
          constraints.put(ev.name, ret)
          ret
        }
        // {x = e} + (e = a) => { x = e, a = e }
        else if (constraints.valueContains(ev.name)) {
          constraints.put(av.name, ev)
          ev
        }
        // {x = a} + (e = a) => { x = a, e = a }
        else if (constraints.valueContains(av.name)) {
          constraints.put(ev.name, av)
          av
        }
        // {} + (e = a) => { e = a }
        else {
          constraints.put(ev.name, av)
          av
        }

      case (ev: TVar, _) =>
        val ret = replace(actual, constraints.internalMap)
        constraints.put(ev.name, ret)
        ret

      case (_, av: TVar) =>
        val ret = replace(expected, constraints.internalMap)
        constraints.put(av.name, ret)
        ret

      case (expectedFunc@TFunc(expectedFrom, expectedTo), actualFunc@TFunc(actualFrom, actualTo)) =>
        if (expectedFrom.length < actualFrom.length) {
          throw new TooManyArguments(expectedFrom.length, actualFrom.length)
        } else if (expectedFrom.length > actualFrom.length) {
          unify2(expectedFunc.partialApply(actualFrom.length), actualFunc, constraints)
        } else {
          val unifiedParams = expectedFrom zip actualFrom map { case (exp, act) =>
            try {
              unify2(exp, act, constraints)
            } catch {
              case error: OccursCheckError =>
                throw new TypeMismatchDueToOccursCheck(error.message, exp, act)
            }
          }
          val ret = unify2(expectedTo, actualTo, constraints)
          // Remember to replace
          TFunc(unifiedParams.map(x => replace(x, constraints.internalMap)), ret)
        }

      case (Generic(expectedName, expectedParams), Generic(actualName, actualParams)) =>
        if (expectedName != actualName || expectedParams.length != actualParams.length) {
          throw new TypeMismatchError(expected, actual)
        }
        val unifiedParams = expectedParams zip actualParams map { case (expectedParamType, actualParamType) =>
          unify2(expectedParamType, actualParamType, constraints)
        }
        Generic(expectedName, unifiedParams.map(x => replace(x, constraints.internalMap)))

      case _ =>
        if (expected != actual) {
          throw new TypeMismatchError(expected, actual)
        }
        expected
    }
  }

  def analyze(context: TypeContext, expr: Expr, constraints: Constraint): Type = {
    expr match {
      // Boolean literal
      case _: LitBool => TBool

      // Int literal
      case _: LitInt => TInt

      // Variable
      case Var(name) =>
        context.get(name) match {
          case Some(t) => inst(context, t)
          case _ => throw new CannotFindVariable(name)
        }

      case If(condition, body, elseBody) =>
        val conditionType = analyze(context, condition, constraints)
        unify(TBool, conditionType, constraints)
        val bodyType = analyze(context, body, constraints)
        val elseType = analyze(context, elseBody, constraints)
        unify(bodyType, elseType, constraints)

      // Function application
      case Apply(func, args) =>
        val funcType = analyze(context, func, constraints)
        val actualArgsType = args.map(x => analyze(context, x, constraints))
        val resultType = newTypeVariable()
        val a = unify(funcType, TFunc(actualArgsType, resultType), constraints)
        a.asInstanceOf[TFunc].to

      // Lambda expression
      case Lambda(argName, body) =>
        val argType = newTypeVariable()
        val retType = analyze(context + (argName -> argType), body, constraints)
        TFunc(Array(constraints.getOrElse(argType.name, argType)), retType)

      // Let expression
      case Let(name, definition, body) =>
        val definitionType = analyze(context, definition, constraints)
        val genDefinitionType = gen(context, definitionType)
        analyze(context + (name -> genDefinitionType), body, constraints)

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
            val exprType = analyze(bindingAnalyzeContext, bindingExpr, constraints)
            val tempType = constraints.getOrElse(tempTVar.name, tempTVar)
            val ret = (name, unify(tempType, exprType, constraints))
            ret
          }

        // Generalize the expressions's types
        var bodyAnalyzeContext = context
        bindingNameAndTypes foreach { case (name, bindingExprType) =>
          bodyAnalyzeContext += (name -> gen(context, bindingExprType))
        }
        analyze(bodyAnalyzeContext, body, constraints)
    }
  }
}

object AnalyzerTest extends App {
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
    "pair" -> TFunc(Array(a, b), Generic("Pair", Array(a, b))),
    "test" -> TFunc(Array(Generic("Pair", Array(TInt, TInt))), TInt)
  )

  val e0 = LitBool(true)
  val e1 = LitInt(1)
  val e2 = Lambda("x", Var("x"))
  val e3 = Apply(e2, Array(e1))
  val e4 = Let("x", e0, Var("x"))
  val e5 = Let("f", Lambda("x", Var("x")), Apply(Var("f"), Array(Var("f"))))
  val e6 = Lambda("y", Let("f", Lambda("x", Apply(Var("x"), Array(Var("y")))), Var("f")))

  val e7 = LetRec(
    Array(
      ("fix",
        Lambda("f",
          Apply(Var("f"),
            Array(Apply(Var("fix"), Array(Var("f")))))))),
    Apply(Var("fix"), Array(Var("fix")))
  )

  val e77 = LetRec(
    Array(
      ("fix",
        Lambda("f",
          Apply(Var("f"),
            Array(Apply(Var("fix"), Array(Var("f")))))))),
    Var("fix")
  )

  val e8 = Apply(Var("pair"), Array(LitInt(1), LitInt(2)))
  val e88 = Apply(Var("pair"), Array(LitInt(1)))
  val e9 = Apply(Var("test"), Array(LitBool(true)))

  val e10 = LetRec(
    Array(
      ("f", Lambda("x", If(LitBool(true), LitInt(1), Apply(Var("g"), Array(LitInt(1)))))),
      ("g", Lambda("x", If(LitBool(true), LitInt(1), Apply(Var("f"), Array(LitInt(1))))))
    ),
    Apply(Var("f"), Array(LitInt(1)))
  )

  val analyzer = new TypeAnalyzer
  val preContext = new TypeContext(prelude)
  val t0 = analyzer.analyze(preContext, e77, new Constraint)
  println(t0)

  val t1 = analyzer.analyze(preContext, e88, new Constraint)
  println(t1)

  val t2 = analyzer.analyze(preContext, e7, new Constraint)
  println(t2)
}