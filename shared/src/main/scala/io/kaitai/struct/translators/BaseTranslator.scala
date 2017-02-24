package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._

abstract class BaseTranslator(val provider: TypeProvider) extends TypeDetector(provider) {
  def translate(v: Ast.expr): String = {
    v match {
      case Ast.expr.IntNum(n) =>
        doIntLiteral(n)
      case Ast.expr.FloatNum(n) =>
        doFloatLiteral(n)
      case Ast.expr.Str(s) =>
        doStringLiteral(s)
      case Ast.expr.Bool(n) =>
        doBoolLiteral(n)
      case Ast.expr.EnumById(enumType, id) =>
        val enumSpec = provider.resolveEnum(enumType.name)
        doEnumById(enumSpec.name, translate(id))
      case Ast.expr.EnumByLabel(enumType, label) =>
        val enumSpec = provider.resolveEnum(enumType.name)
        doEnumByLabel(enumSpec.name, label.name)
      case Ast.expr.Name(name: Ast.identifier) =>
        doLocalName(name.name)
      case Ast.expr.UnaryOp(op: Ast.unaryop, v: Ast.expr) =>
        s"${unaryOp(op)}${translate(v)}"
      case Ast.expr.Compare(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) =>
        (detectType(left), detectType(right)) match {
          case (_: NumericType, _: NumericType) =>
            doNumericCompareOp(left, op, right)
          case (_: BooleanType, _: BooleanType) =>
            op match {
              case Ast.cmpop.Eq | Ast.cmpop.NotEq =>
                // FIXME: probably for some languages we'll need non-numeric comparison
                doNumericCompareOp(left, op, right)
              case _ =>
                throw new TypeMismatchError(s"can't compare booleans using $op operator")
            }
          case (_: StrType, _: StrType) =>
            doStrCompareOp(left, op, right)
          case (EnumType(ltype, _), EnumType(rtype, _)) =>
            if (ltype != rtype) {
              throw new TypeMismatchError(s"can't compare enums type ${ltype} and ${rtype}")
            } else {
              doEnumCompareOp(left, op, right)
            }
          case (ltype, rtype) =>
            throw new RuntimeException(s"can't compare ${ltype} and ${rtype}")
        }
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        (detectType(left), detectType(right), op) match {
          case (_: NumericType, _: NumericType, _) =>
            numericBinOp(left, op, right)
          case (_: StrType, _: StrType, Ast.operator.Add) =>
            strConcat(left, right)
          case (ltype, rtype, _) =>
            throw new RuntimeException(s"can't do ${ltype} ${op} ${rtype}")
        }
      case Ast.expr.BoolOp(op: Ast.boolop, values: Seq[Ast.expr]) =>
        doBooleanOp(op, values)
      case Ast.expr.IfExp(condition, ifTrue, ifFalse) =>
        doIfExp(condition, ifTrue, ifFalse)
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        detectType(idx) match {
          case _: IntType =>
            doSubscript(container, idx)
          case idxType =>
            throw new RuntimeException(s"can't use $idx as array index (need int, got $idxType)")
        }
      case Ast.expr.Attribute(value: Ast.expr, attr: Ast.identifier) =>
        val valType = detectType(value)
        valType match {
          case _: UserType =>
            userTypeField(value, attr.name)
          case _: StrType =>
            attr.name match {
              case "length" => strLength(value)
              case "reverse" => strReverse(value)
              case "to_i" => strToInt(value, Ast.expr.IntNum(10))
            }
          case _: IntType =>
            attr.name match {
              case "to_s" => intToStr(value, Ast.expr.IntNum(10))
            }
          case ArrayType(inType) =>
            attr.name match {
              case "first" => arrayFirst(value)
              case "last" => arrayLast(value)
              case "size" => arraySize(value)
            }
          case KaitaiStreamType =>
            attr.name match {
              case "size" => kaitaiStreamSize(value)
              case "eof" => kaitaiStreamEof(value)
              case "pos" => kaitaiStreamPos(value)
            }
          case et: EnumType =>
            attr.name match {
              case "to_i" => enumToInt(value, et)
              case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
            }
        }
      case Ast.expr.Call(func: Ast.expr, args: Seq[Ast.expr]) =>
        func match {
          case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) =>
            val objType = detectType(obj)
            (objType, methodName.name) match {
              // TODO: check argument quantity
              case (_: StrType, "substring") => strSubstring(obj, args(0), args(1))
              case (_: StrType, "to_i") => strToInt(obj, args(0))
              case (_: BytesType, "to_s") => bytesToStr(translate(obj), args(0))
              case _ => throw new RuntimeException(s"don't know how to call method '$methodName' of object type '$objType'")
            }
        }
      case Ast.expr.List(values: Seq[Ast.expr]) =>
        val t = detectArrayType(values)
        t match {
          case Int1Type(_) =>
            val literalBytes: Seq[Byte] = values.map {
              case Ast.expr.IntNum(x) =>
                if (x < 0 || x > 0xff) {
                  throw new TypeMismatchError(s"got a weird byte value in byte array: $x")
                } else {
                  x.toByte
                }
              case n =>
                throw new RuntimeException(s"got $n in byte array, unable to put it literally")
            }
            doByteArrayLiteral(literalBytes)
          case _ =>
            doArrayLiteral(t, values)
        }
      case Ast.expr.CastToType(value, typeName) =>
        doCast(value, typeName.name)
    }
  }

  def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    s"(${translate(left)} ${binOp(op)} ${translate(right)})"
  }

  def binOp(op: Ast.operator): String = {
    op match {
      case Ast.operator.Add => "+"
      case Ast.operator.Sub => "-"
      case Ast.operator.Mult => "*"
      case Ast.operator.Div => "/"
      case Ast.operator.Mod => "%"
      case Ast.operator.BitAnd => "&"
      case Ast.operator.BitOr => "|"
      case Ast.operator.BitXor => "^"
      case Ast.operator.LShift => "<<"
      case Ast.operator.RShift => ">>"
    }
  }

  def doNumericCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
  }

  def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
  }

  def doEnumCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
  }

  def cmpOp(op: Ast.cmpop): String = {
    op match {
      case Ast.cmpop.Lt => "<"
      case Ast.cmpop.LtE => "<="
      case Ast.cmpop.Gt => ">"
      case Ast.cmpop.GtE => ">="
      case Ast.cmpop.Eq => "=="
      case Ast.cmpop.NotEq => "!="
    }
  }

  def doBooleanOp(op: Ast.boolop, values: Seq[Ast.expr]): String = {
    val opStr = s"${booleanOp(op)}"
    val dividerStr = s") ${opStr} ("
    val valuesStr = values.map(translate).mkString("(", dividerStr, ")")

    // Improve compatibility for statements like: ( ... && ... || ... ) ? ... : ...
    s" (${valuesStr}) "
  }

  def booleanOp(op: Ast.boolop) = op match {
    case Ast.boolop.Or => "||"
    case Ast.boolop.And => "&&"
  }

  def unaryOp(op: Ast.unaryop) = op match {
    case Ast.unaryop.Invert => "~"
    case Ast.unaryop.Minus => "-"
    case Ast.unaryop.Not => "!"
  }

  def doSubscript(container: Ast.expr, idx: Ast.expr): String
  def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String
  def doCast(value: Ast.expr, typeName: String): String = translate(value)

  // Literals
  def doIntLiteral(n: BigInt): String = n.toString
  def doFloatLiteral(n: Any): String = n.toString
  def doStringLiteral(s: String): String = "\"" + s + "\""
  def doBoolLiteral(n: Boolean): String = n.toString
  def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): String = "[" + value.map((v) => translate(v)).mkString(", ") + "]"
  def doByteArrayLiteral(arr: Seq[Byte]): String = "[" + arr.map(_ & 0xff).mkString(", ") + "]"

  def doLocalName(s: String): String = doName(s)
  def doName(s: String): String
  def userTypeField(value: Ast.expr, attrName: String): String =
    s"${translate(value)}.${doName(attrName)}"
  def doEnumByLabel(enumTypeAbs: List[String], label: String): String
  def doEnumById(enumTypeAbs: List[String], id: String): String

  // Predefined methods of various types
  def strConcat(left: Ast.expr, right: Ast.expr): String = s"${translate(left)} + ${translate(right)}"
  def strToInt(s: Ast.expr, base: Ast.expr): String
  def enumToInt(value: Ast.expr, et: EnumType): String
  def intToStr(i: Ast.expr, base: Ast.expr): String
  def bytesToStr(bytesExpr: String, encoding: Ast.expr): String
  def strLength(s: Ast.expr): String
  def strReverse(s: Ast.expr): String
  def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String

  def arrayFirst(a: Ast.expr): String
  def arrayLast(a: Ast.expr): String
  def arraySize(a: Ast.expr): String

  def kaitaiStreamSize(value: Ast.expr): String = userTypeField(value, "size")
  def kaitaiStreamEof(value: Ast.expr): String = userTypeField(value, "is_eof")
  def kaitaiStreamPos(value: Ast.expr): String = userTypeField(value, "pos")
}
