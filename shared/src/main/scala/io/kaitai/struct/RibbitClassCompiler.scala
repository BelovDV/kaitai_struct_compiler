package io.kaitai.struct

import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{
  LanguageCompiler,
  LanguageCompilerStatic
}
import io.kaitai.struct.precompile.CalculateSeqSizes
import io.kaitai.struct.translators.RibbitTranslator

import scala.collection.mutable.ListBuffer

class RibbitClassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec)
    extends AbstractCompiler {
  import RibbitClassCompiler._

  val out = new StringLanguageOutputWriter("\t")

  val provider = new ClassTypeProvider(classSpecs, topClass)
  val translator = new RibbitTranslator(provider)

  def nowClass: ClassSpec = provider.nowClass
  def nowClassName = provider.nowClass.name
  var currentTable: String = ""

  override def compile: CompileLog.SpecSuccess = {

    compileClass(topClass)

    out.puts("/// Fix-sized list of elements of format `T`.")
    out.puts("data list(len: '64, T: data) {")
    out.inc
    out.puts("if 'ne(len, 0) {")
    out.inc
    out.puts("head: T,")
    out.puts("tail: list('dec(len), T),")
    out.dec
    out.puts("}\n}")
    out.dec

    CompileLog.SpecSuccess(
      "",
      List(
        CompileLog.FileSuccess(
          outFileName(topClass.nameAsStr),
          out.result
        )
      )
    )
  }

  def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass
    val className = curClass.name

    universalDoc(curClass.doc)
    debug(className)
    out.puts(s"data ${className.last} {")
    out.inc

    CalculateSeqSizes.forEachSeqAttr(
      curClass,
      (attr, seqPos, sizeElement, sizeContainer) => {
        outField(attr, attrName(attr), attr2type(attr))
      }
    )

    out.dec
    out.puts("}")
    out.puts("")

    curClass.types.foreach { case (_, intClass) => compileClass(intClass) }
  }

  def outField(attr: AttrSpec, name: String, attr_type: String): Unit = {
    attr.cond.repeat match {
      case RepeatExpr(expr) =>
        out.puts("/// RepeatExpr")
        def rep = translator.translate(expr)
        out.puts(name + ": list(" + rep + ", " + attr_type + "),")
      case RepeatUntil(ex) =>
        out.puts("ERROR: Repeat until")
      case RepeatEos =>
        out.puts("ERROR: Repeat eos")
      case NoRepeat =>
        out.puts(name + ": " + attr_type + ",")
    }
  }

  def attrName(attr: AttrSpec): String = {
    attr.id match {
      case NamedIdentifier(name) => name
      case _                     => "ERROR in attrName"
    }
  }

  def attr2type(attr: AttrSpec): String = {
    debug(attr)
    universalDoc(attr.doc)
    if (attr.valid.isDefined) {
      out.puts("/// condition")
      out.puts("/// " + attr.valid.get.toString())
    }
    data2type(attr.dataType)
  }
  def data2type(data: DataType): String = {
    debug(data)
    data match {
      case Int1Type(_) => bytes_int(1, None)
      case IntMultiType(signed, width, endian) =>
        bytes_int(width.width, endian)
      case FloatMultiType(width, endian) =>
        bytes_int(width.width, endian)
      case StrFromBytesType(bytes, encoding) => {
        out.puts("/// StrFromBytesType" + encoding)
        bytes2type(bytes)
      }
      case ut: UserType =>
        if (ut.name.length != 1) {
          "ERROR in attrToString: UserType " + ut.name.length.toString
        } else {
          ut.name.head + "()"
        }
      case bytes: BytesType => bytes2type(bytes)
      case _                => "ERROR in attrToString: _ " + data.toString
    }
  }
  def bytes2type(bytes: BytesType): String = {
    debug(bytes)
    bytes match {
      case BytesLimitType(size, terminator, include, padRight, process) =>
        bytes_str(translator.translate(size), None)
      case _ => "ERROR bytes2type " + bytes.toString
    }
  }
  def bytes_int(bytes: Int, endian: Option[FixedEndian]): String =
    bytes_str(bytes.toString, endian)
  def bytes_str(
      bytes: String,
      endian: Option[FixedEndian]
  ): String = {
    endian match {
      case Some(LittleEndian) => "bytes(" + bytes + ", little) as '64"
      case Some(BigEndian)    => "bytes(" + bytes + ", big) as '64"
      case None               => "bytes(" + bytes + ", any) as '64"
    }
  }

  def outFileName(topClassName: String): String = s"$topClassName.gf"
  def debug(info: Any): Unit = out.puts("\t\t\t\t\t\t\t\t/// " + info)
  def universalDoc(doc: DocSpec): Unit = {
    if (doc.summary.isDefined) {
      out.puts("/// DOC:")
      doc.summary.foreach((summary) => out.puts("/// - " + summary))
    }
  }
}

object RibbitClassCompiler extends LanguageCompilerStatic {
  // FIXME: Unused, should be probably separated from LanguageCompilerStatic
  override def getCompiler(
      tp: ClassTypeProvider,
      config: RuntimeConfig
  ): LanguageCompiler = ???
}
