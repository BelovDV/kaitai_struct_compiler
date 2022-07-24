package io.kaitai.struct.languages

import io.kaitai.struct.languages.components._
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig}
import io.kaitai.struct.translators.RibbitTranslator
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{
  DataType,
  EndOfStreamError,
  FixedEndian,
  InheritedEndian,
  KSError,
  UndecidedEndiannessError,
  NeedRaw
}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.PythonTranslator
import io.kaitai.struct.{
  ClassTypeProvider,
  RuntimeConfig,
  StringLanguageOutputWriter,
  Utils
}
import io.kaitai.struct.translators.AbstractTranslator
import io.kaitai.struct.datatype.Endianness

class RibbitCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
    extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile {

  override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] =
    List()

  override def switchStart(id: Identifier, on: expr): Unit = System.err.println(20)

  override def switchCaseStart(condition: expr): Unit = System.err.println(22)

  override def switchCaseEnd(): Unit = System.err.println(24)

  override def switchElseStart(): Unit = System.err.println(26)

  override def switchEnd(): Unit = System.err.println(28)

  override def ksErrorName(err: KSError): String = {
    System.err.println(30)
    ""
  }

  override def handleAssignmentTempVar(
      dataType: DataType,
      id: String,
      expr: String
  ): Unit = System.err.println(32)

  override val translator = new RibbitTranslator(typeProvider, importList)

  override def indent: String = {
    System.err.println("indent")
    "    "
  }

  override def outFileName(topClassName: String): String = {
    System.err.println(42)
    s"$topClassName.gf"
  }

  override def type2class(className: String): String = {
    System.err.println(44)
    ""
  }

  override def fileHeader(topClassName: String): Unit = System.err.println("fileHeader")

  override def classHeader(name: List[String]): Unit = {
    System.err.println("classHeader")
    out.puts(s"data ${name.last}() {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    System.err.println("classFooter")
    out.dec
    out.puts(s"}")
  }

  override def classConstructorHeader(
      name: List[String],
      parentType: DataType,
      rootClassName: List[String],
      isHybrid: Boolean,
      params: List[ParamDefSpec]
  ): Unit = System.err.println("classConstructorHeader")

  override def classConstructorFooter: Unit = System.err.println(54)

  override def runRead(name: List[String]): Unit = System.err.println(56)

  override def runReadCalc(): Unit = System.err.println(58)

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit =
    System.err.println(60)

  override def readFooter(): Unit = System.err.println(62)

  override def attributeDeclaration(
      attrName: Identifier,
      attrType: DataType,
      isNullable: Boolean
  ): Unit = System.err.println(64)

  override def attributeReader(
      attrName: Identifier,
      attrType: DataType,
      isNullable: Boolean
  ): Unit = System.err.println(66)

  override def attrParse(
      attr: AttrLikeSpec,
      id: Identifier,
      defEndian: Option[Endianness]
  ): Unit = System.err.println(68)

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit =
    System.err.println(70)

  override def attrFixedContentsParse(
      attrName: Identifier,
      contents: Array[Byte]
  ): Unit = System.err.println(72)

  override def condIfHeader(expr: expr): Unit = System.err.println(74)

  override def condIfFooter(expr: expr): Unit = System.err.println(76)

  override def condRepeatCommonInit(
      id: Identifier,
      dataType: DataType,
      needRaw: NeedRaw
  ): Unit = System.err.println(78)

  override def condRepeatEosHeader(
      id: Identifier,
      io: String,
      dataType: DataType
  ): Unit = System.err.println(80)

  override def condRepeatEosFooter: Unit = System.err.println(82)

  override def condRepeatExprHeader(
      id: Identifier,
      io: String,
      dataType: DataType,
      repeatExpr: expr
  ): Unit = System.err.println(84)

  override def condRepeatExprFooter: Unit = System.err.println(86)

  override def condRepeatUntilHeader(
      id: Identifier,
      io: String,
      dataType: DataType,
      untilExpr: expr
  ): Unit = System.err.println(88)

  override def condRepeatUntilFooter(
      id: Identifier,
      io: String,
      dataType: DataType,
      untilExpr: expr
  ): Unit = System.err.println(90)

  override def attrProcess(
      proc: ProcessExpr,
      varSrc: Identifier,
      varDest: Identifier,
      rep: RepeatSpec
  ): Unit = System.err.println(92)

  override def normalIO: String = {
    System.err.println(94)
    ""
  }

  override def useIO(ioEx: expr): String = {
    System.err.println(96)
    ""
  }

  override def pushPos(io: String): Unit = System.err.println(98)

  override def seek(io: String, pos: expr): Unit = System.err.println(100)

  override def popPos(io: String): Unit = System.err.println(102)

  override def alignToByte(io: String): Unit = System.err.println(104)

  override def instanceHeader(
      className: List[String],
      instName: InstanceIdentifier,
      dataType: DataType,
      isNullable: Boolean
  ): Unit = System.err.println(106)

  override def instanceFooter: Unit = System.err.println(108)

  override def instanceCheckCacheAndReturn(
      instName: InstanceIdentifier,
      dataType: DataType
  ): Unit = System.err.println(110)

  override def instanceReturn(
      instName: InstanceIdentifier,
      attrType: DataType
  ): Unit = System.err.println(112)

  override def instanceCalculate(
      instName: Identifier,
      dataType: DataType,
      value: expr
  ): Unit = System.err.println(114)

  override def enumDeclaration(
      curClass: List[String],
      enumName: String,
      enumColl: Seq[(Long, EnumValueSpec)]
  ): Unit = System.err.println(116)

  override def blockScopeHeader: Unit = {
    System.err.println("blockScopeHeader")
  }
  override def blockScopeFooter: Unit = {
    System.err.println("blockScopeFooter")
  }

  override def innerClasses: Boolean = false

  import RibbitCompiler._
}

object RibbitCompiler extends LanguageCompilerStatic {
  override def getCompiler(
      tp: ClassTypeProvider,
      config: RuntimeConfig
  ): LanguageCompiler = new RibbitCompiler(tp, config)
}
