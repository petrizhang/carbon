package com.pzque.coco.parser

import com.pzque.coco.ast.AST

import scala.util.parsing.input.Reader

trait SkyParser {
  def parse(reader: Reader[Char]): Either[String, AST]

  def parse(code: String): Either[String, AST]
}
