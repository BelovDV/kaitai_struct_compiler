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

  var counter: Int = 0

  override def compile: CompileLog.SpecSuccess = {

    compileClass(topClass)

    out.puts("/// Fix-sized list of elements of format `T`.")
    out.puts("data std_list(len: '64, T: data) {")
    out.inc
    out.puts("if 'ne(len, 0) {")
    out.inc
    out.puts("head: T,")
    out.puts("tail: std_list('dec(len), T),")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")

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

    curClass.instances.foreach { case (instName, _) =>
      out.puts("/// ERROR: instance " + instName.name)
    }

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
    val t = attr.cond.repeat match {
      case RepeatExpr(expr) =>
        out.puts("/// RepeatExpr")
        def rep = translator.translate(expr)
        "std_list(" + rep + ", " + attr_type + ")"
      case RepeatUntil(ex) =>
        "ERROR: Repeat until"
      case RepeatEos =>
        "ERROR: Repeat eos"
      case NoRepeat =>
        attr_type
    }
    if (attr.cond.ifExpr.isDefined) {
      out.puts(name + ": if " + "condition" + " {")
      out.inc
      out.puts(t)
      out.dec
      out.puts("},")
    } else {
      out.puts(name + ": " + t + ",")
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
      out.puts("/// valid")
      out.puts("/// " + attr.valid.get.toString)
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
          "ERROR in data2type: UserType " + ut.name.length.toString
        } else {
          ut.name.head + "()"
        }
      case bytes: BytesType => bytes2type(bytes)
      case st: SwitchType => {
        st.cases.foreach { case (caseExpr, caseType) =>
          debug("This may be done by creating new type with if")
          debug("To do so should be created func to create type")
          debug("   or set of additional types")
        }
        ""
      }
      case _ => "ERROR in data2type: _ " + data.toString
    }
  }
  def bytes2type(bytes: BytesType): String = {
    debug(bytes)
    bytes match {
      case BytesLimitType(size, terminator, include, padRight, process) =>
        if (terminator.isDefined) {
          out.puts("/// MAYBE ERROR: BytesLimitType terminator")
        }
        if (padRight.isDefined) {
          out.puts("/// FIXME: BytesLimitType padRight")
        }
        if (!include) {
          out.puts("/// MAYBE ERROR: BytesLimitType not include")
        }
        bytes_str(translator.translate(size), None)
      case BytesTerminatedType(
            terminator,
            include,
            consume,
            eosError,
            process
          ) =>
        debug("FIXME: include, consume")
        debug("UNKNOWN: eosError")
        "std_bytes_terminated(" + terminator.toString + ")"
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
