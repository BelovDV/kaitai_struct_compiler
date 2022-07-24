package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.RibbitCompiler

class RibbitTranslator(provider: TypeProvider, importList: ImportList)
    extends BaseTranslator(provider) {

  override def strLength(s: Ast.expr): String = {
    println(10013)
    ""
  }

  override def strReverse(s: Ast.expr): String = {
    println(10018)
    ""
  }

  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    println(10023)
    ""
  }

  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String = {
    println(10028)
    ""
  }

  override def intToStr(value: Ast.expr, num: Ast.expr): String = {
    println(10033)
    ""
  }

  override def floatToInt(value: Ast.expr): String = {
    println(10038)
    ""
  }

  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String = {
    println(10043)
    ""
  }

  override def arrayFirst(a: Ast.expr): String = {
    println(10048)
    ""
  }

  override def arrayLast(a: Ast.expr): String = {
    println(10053)
    ""
  }

  override def arraySize(a: Ast.expr): String = {
    println(10058)
    ""
  }

  override def arrayMin(a: Ast.expr): String = {
    println(10063)
    ""
  }

  override def arrayMax(a: Ast.expr): String = {
    println(10068)
    ""
  }

  override def enumToInt(value: Ast.expr, et: EnumType): String = {
    println(10073)
    ""
  }

  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String = {
    println(10078)
    ""
  }

  override def doName(s: String): String = {
    System.err.println(10083, s)
    s
  }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = {
    println(10088)
    ""
  }

  override def doEnumById(enumTypeAbs: List[String], id: String): String = {
    println(10093)
    ""
  }

  override def bytesToStr(value: String, expr: Ast.expr): String = {
    println(10098)
    ""
  }
}
