package com.pzque.coco

import javax.script.{ScriptEngine, ScriptEngineManager}

import scala.tools.nsc.interpreter.IMain

object App extends App {
  val engine: ScriptEngine = new ScriptEngineManager().getEngineByName("scala")
  val iMain: IMain = engine.asInstanceOf[IMain]

  val settings = iMain.settings
  settings.usejavacp.value = true
  var a = 10
  engine.put("a", a)
  engine.eval("println(a)")
  a = 100
  engine.eval("println(a)")
}