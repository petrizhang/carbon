package com.pzque.coco.errors

class UndefinedVariable(variableName: String)
  extends RuntimeError(s"Undefined variable '$variableName'.")
