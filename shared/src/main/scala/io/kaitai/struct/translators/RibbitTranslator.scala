package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.datatype.DataType.EnumType
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier

class RibbitTranslator(provider: TypeProvider)
    extends BaseTranslator(provider)
    with ByteArraysAsTrueArrays[String] {
  override def doIfExp(
      condition: Ast.expr,
      ifTrue: Ast.expr,
      ifFalse: Ast.expr
  ): String = "ERROR: doIfExp"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    "ERROR: doByteArrayNonLiteral"
  override def doName(s: String): String = s // TODO: check
  override def doInternalName(id: Identifier): String = "ERROR: doInternalName"
  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    "ERROR: doEnumByLabel"
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    "ERROR: doEnumById"
  override def bytesToStr(value: String, expr: Ast.expr): String =
    "ERROR: bytesToStr"

  override def numericBinOp(
      left: Ast.expr,
      op: Ast.operator,
      right: Ast.expr
  ) = {
    s"'${binOp(op)}(${translate(right)}, ${translate(left)})"
  }
  override def binOp(op: Ast.operator): String = {
    op match {
      case Ast.operator.Add => "add"
      case Ast.operator.Sub => "sub"
      case Ast.operator.Mult => "mus"
      case Ast.operator.Div => "/"
      case Ast.operator.Mod => "%"
      case Ast.operator.BitAnd => "&"
      case Ast.operator.BitOr => "|"
      case Ast.operator.BitXor => "^"
      case Ast.operator.LShift => "<<"
      case Ast.operator.RShift => ">>"
    }
  }
  override def unaryOp(op: Ast.unaryop): String = op match {
    case Ast.unaryop.Invert => "'~"
    case Ast.unaryop.Minus => "'-"
    case Ast.unaryop.Not => "'ne"
  }

  override def arrayFirst(a: Ast.expr): String = "ERROR: arrayFirst"
  override def arrayLast(a: Ast.expr): String = "ERROR: arrayLast"
  override def arrayMax(a: Ast.expr): String = "ERROR: arrayMax"
  override def arrayMin(a: Ast.expr): String = "ERROR: arrayMin"
  override def arraySize(a: Ast.expr): String = "ERROR: arraySize"
  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String =
    "ERROR: arraySubscript"
  override def enumToInt(value: Ast.expr, et: DataType.EnumType): String =
    "ERROR: enumToInt"
  override def floatToInt(value: Ast.expr): String = "ERROR: floatToInt"
  override def intToStr(value: Ast.expr, num: Ast.expr): String =
    "ERROR: intToStr"
  override def strLength(s: Ast.expr): String = "ERROR: strLength"
  override def strReverse(s: Ast.expr): String = "ERROR: strReverse"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    "ERROR: strSubstring"
  override def strToInt(s: Ast.expr, base: Ast.expr): String = "ERROR: strToInt"
}
