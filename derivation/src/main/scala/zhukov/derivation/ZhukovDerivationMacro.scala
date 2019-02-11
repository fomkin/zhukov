package zhukov.derivation

import macrocompat.bundle
import zhukov.protobuf.WireFormat

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

@bundle
class ZhukovDerivationMacro(val c: blackbox.Context) {

  import c.universe._

  def unmarshallerImpl[T: WeakTypeTag]: Tree = {
    val T = weakTypeTag[T].tpe
    val ts = T.typeSymbol
    if (ts.isClass && ts.asClass.isCaseClass) {
      val companion = ts.companion
      caseClassUnmarshaller(T, companion)
    } else if (ts.isClass && ts.asClass.isTrait && ts.asClass.isSealed) {
      sealedTraitUnmarshaller(T, ts.asClass)
    }
    else c.abort(c.enclosingPosition, "Zhukov derivation is supported only for case classes and sealed traits")
  }

  private def getOrAbort[T](x: Option[T], pos: Position, msg: String) = x match {
    case None => c.abort(pos, msg)
    case Some(value) => value
  }

  private def inferWireType(tpe: c.universe.Type) = {
    if (c.typecheck(q"implicitly[zhukov.Unmarshaller.VarintUnmarshaller[$tpe]]", silent = true).tpe != NoType) VarInt
    else if (c.typecheck(q"implicitly[zhukov.Unmarshaller.Fixed32Unmarshaller[$tpe]]", silent = true).tpe != NoType) Fixed32
    else if (c.typecheck(q"implicitly[zhukov.Unmarshaller.Fixed64Unmarshaller[$tpe]]", silent = true).tpe != NoType) Fixed64
    else if (c.typecheck(q"implicitly[zhukov.Unmarshaller.CodedUnmarshaller[$tpe]]", silent = true).tpe != NoType) Coded
    else LengthDelimited
  }

  private def commonUnmarshaller(T: Type, fields: List[Field]): Tree = {
    val vars = fields.groupBy(_.varName).mapValues(_.head).collect {
      case (name, Field(_, _, _, Some(default), repTpe, Some(tpe), None)) =>
        q"var $name = ${repTpe.typeSymbol.companion}.newBuilder[$tpe] ++= $default"
      case (name, Field(_, _, _, Some(default), _, None, None)) =>
        q"var $name = $default"
      case (name, Field(_, _, _, None, _, None, Some(parent))) =>
        q"var $name:$parent = null"
    }
    val cases = fields.flatMap { x =>
      val tpe = x.repTpe.getOrElse(x.tpe)
      val wireType = inferWireType(tpe)
      val tag = WireFormat.makeTag(x.index, wireType.value)
      val singleRead = q"implicitly[zhukov.Unmarshaller[$tpe]].read(_stream)"
      if (x.repTpe.isEmpty) {
        wireType match {
          case LengthDelimited =>
            List(
              cq"""$tag =>
              val _length = _stream.readRawVarint32()
              val _oldLimit = _stream.pushLimit(_length)
              ${x.varName} = $singleRead
              _stream.checkLastTagWas(0)
              _stream.popLimit(_oldLimit)
            """)
          case _ =>
            List(cq"$tag => ${x.varName} = $singleRead")
        }
      } else {
        wireType match {
          case VarInt | Fixed32 | Fixed64 => // Packed
            val repTag = WireFormat.makeTag(x.index, WireFormat.WIRETYPE_LENGTH_DELIMITED)
            val `case` =
              cq"""
              $repTag =>
                val _length = _stream.readRawVarint32()
                val _oldLimit = _stream.pushLimit(_length)
                while (_stream.getBytesUntilLimit > 0)
                  ${x.varName} += $singleRead
                _stream.popLimit(_oldLimit)
            """
            List(`case`, cq"$tag => ${x.varName} += $singleRead")
          case Coded =>
            List(cq"$tag => ${x.varName} += $singleRead")
          case LengthDelimited =>
            List(
              cq"""$tag =>
              val _length = _stream.readRawVarint32()
              val _oldLimit = _stream.pushLimit(_length)
              ${x.varName} += $singleRead
              _stream.checkLastTagWas(0)
              _stream.popLimit(_oldLimit)
            """)
        }
      }
    }

    q"""
      var _done = false
      ..$vars
      while (!_done) {
        val _tag = _stream.readTag()
        (_tag: @scala.annotation.switch) match {
          case 0 => _done = true
          case ..$cases
          case _ => _stream.skipField(_tag)
        }
      }
    """
  }

  private def sealedTraitUnmarshaller(T: Type, ts: ClassSymbol): Tree = {
    val children = ts.knownDirectSubclasses.toList
    val termName = TermName("_value")
    val fields = children.zipWithIndex.map {
      case (x, i) =>
        Field(
          index = i + 1,
          originalName = None,
          varName = termName,
          default = None,
          tpe = x.asClass.toType,
          repTpe = None,
          parentType = Some(T)
        )
    }
    q"""
      new zhukov.Unmarshaller[$T] {
        def read(_stream: zhukov.protobuf.CodedInputStream) = {
          ..${commonUnmarshaller(T, fields)}
          _value
        }
      }
    """
  }

  private def caseClassUnmarshaller(T: Type, module: Symbol): Tree = {
    val constructor = module.typeSignature.decl(applyName).asMethod
    val params = getOrAbort(constructor.paramLists.headOption, constructor.pos, "Case class should have parameters")
    val fields = params.zipWithIndex.map {
      case (param, i) =>
        val defaultValue = TermName(s"apply$$default$$${i + 1}") // apply$default$1
      val tpe = param.typeSignature
        if (!module.typeSignature.decls.exists(_.name == defaultValue))
          c.abort(param.pos, "Parameter should have default value")
        Field(
          index = i + 1,
          originalName = Some(param.name.toTermName),
          varName = TermName("_" + param.name.toString),
          default = Some(q"$module.$defaultValue"),
          tpe = tpe,
          repTpe =
            if (tpe <:< typeOf[Iterable[_]]) Some(tpe.typeArgs.head)
            else None,
          parentType = None
        )
    }
    val applyArgs = fields.collect {
      case Field(_, Some(originalName), varName, _, _, Some(_), _) =>
        q"$originalName = $varName.result()"
      case Field(_, Some(originalName), varName, _, _, None, _) =>
        q"$originalName = $varName"
    }
    q"""
      new zhukov.Unmarshaller[$T] {
        def read(_stream: zhukov.protobuf.CodedInputStream) = {
          ..${commonUnmarshaller(T, fields)}
          $module.apply(..$applyArgs)
        }
      }
    """
  }

  private val applyName = TermName("apply")

  private sealed abstract class WireType(val value: Int)

  private case object VarInt extends WireType(WireFormat.WIRETYPE_VARINT)

  private case object Fixed64 extends WireType(WireFormat.WIRETYPE_FIXED64)

  private case object Fixed32 extends WireType(WireFormat.WIRETYPE_FIXED32)

  private case object Coded extends WireType(WireFormat.WIRETYPE_LENGTH_DELIMITED)

  private case object LengthDelimited extends WireType(WireFormat.WIRETYPE_LENGTH_DELIMITED)

  private final case class Field(index: Int,
                                 originalName: Option[TermName],
                                 varName: TermName,
                                 default: Option[Tree],
                                 tpe: Type,
                                 repTpe: Option[Type],
                                 parentType: Option[Type])

}
