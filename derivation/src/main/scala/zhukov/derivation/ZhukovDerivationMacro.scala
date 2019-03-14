package zhukov.derivation

import macrocompat.bundle
import zhukov.protobuf.{CodedOutputStream, WireFormat}

import scala.collection.concurrent.TrieMap
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

@bundle
class ZhukovDerivationMacro(val c: blackbox.Context) {

  import c.universe._
  import WireType._

  private val marshallerCache = TrieMap.empty[Type, Tree]
  private val unmarshallerCache = TrieMap.empty[Type, Tree]
  private val sizeMeterCache = TrieMap.empty[Type, Tree]

  def unmarshallerImpl[T: WeakTypeTag]: Tree = {
    val T = weakTypeTag[T].tpe
    val ts = T.typeSymbol
    lazy val tree =
      if (ts.isClass && ts.asClass.isCaseClass) {
        val companion = ts.companion
        caseClassUnmarshaller(T, companion)
      } else if (ts.isClass && ts.asClass.isTrait && ts.asClass.isSealed) {
        sealedTraitUnmarshaller(T, ts.asClass)
      } else {
        c.abort(c.enclosingPosition, onlyCaseClassesAndSealedTraitsSupported(show(T)))
      }
    unmarshallerCache.getOrElseUpdate(T, tree)
  }

  def marshallerImpl[T: WeakTypeTag]: Tree = {
    val T = weakTypeTag[T].tpe
    val ts = T.typeSymbol
    lazy val tree =
      if (ts.isClass && ts.asClass.isCaseClass) {
        val companion = ts.companion
        caseClassMarshaller(T, companion)
      } else if (ts.isClass && ts.asClass.isTrait && ts.asClass.isSealed) {
        sealedTraitMarshaller(T, ts.asClass)
      } else c.abort(c.enclosingPosition, onlyCaseClassesAndSealedTraitsSupported(show(T)))
    marshallerCache.getOrElseUpdate(T, tree)
  }

  def sizeMeterImpl[T: WeakTypeTag]: Tree = {
    val T = weakTypeTag[T].tpe
    val ts = T.typeSymbol
    lazy val tree =
      if (ts.isClass && ts.asClass.isCaseClass) {
        val companion = ts.companion
        caseClassSizeMeter(T, companion)
      } else if (ts.isClass && ts.asClass.isTrait && ts.asClass.isSealed) {
        sealedTraitSizeMeter(T, ts.asClass)
      } else c.abort(c.enclosingPosition, onlyCaseClassesAndSealedTraitsSupported(show(T)))
    sizeMeterCache.getOrElseUpdate(T, tree)
  }

  def unmarshallerLowPriorityImpl[T: WeakTypeTag]: Tree =
    q"zhukov.derivation.LowPriority(${unmarshallerImpl[T]})"

  def marshallerLowPriorityImpl[T: WeakTypeTag]: Tree =
    q"zhukov.derivation.LowPriority(${marshallerImpl[T]})"

  def sizeMeterLowPriorityImpl[T: WeakTypeTag]: Tree =
    q"zhukov.derivation.LowPriority(${sizeMeterImpl[T]})"

  private def caseClassMarshaller(T: Type, module: Symbol) = {
    val fields = resolveCaseClassFields(T, module)
    val writes = fields.map(commonMarshaller(T, _))
    q"""
      zhukov.Marshaller[$T] { (_stream: zhukov.protobuf.CodedOutputStream, _value: $T) =>
        ..$writes
        ()
      }
    """
  }

  private def sealedTraitMarshaller(T: Type, ts: ClassSymbol): Tree = {
    val fields = resolveSealedTraitFields(T, ts)
    val writes = fields.map(x => cq"_v: ${x.tpe} => ${commonMarshaller(T, x)}")
    q"""
      zhukov.Marshaller[$T] { (_stream: zhukov.protobuf.CodedOutputStream, _value: $T) =>
        _value match { case ..$writes }
        ()
      }
    """
  }

  private def caseClassSizeMeter(T: Type, module: Symbol) = {
    val fields = resolveCaseClassFields(T, module)
    val meters = fields.map(commonSizeMeter)
    q"""
      zhukov.SizeMeter[$T] { (_value: $T) =>
        var _size = 0
        ..$meters
        _size
      }
    """
  }

  private def sealedTraitSizeMeter(T: Type, ts: ClassSymbol): Tree = {
    val fields = resolveSealedTraitFields(T, ts)
    val meters = fields.map(x => cq"_v: ${x.tpe} => ${commonSizeMeter(x)}")
    q"""
      zhukov.SizeMeter[$T] { (_value: $T) =>
        var _size = 0
        _value match { case ..$meters }
        _size
      }
    """
  }

  private def resolveSealedTraitFields(T: Type, ts: ClassSymbol) = {
    val children = ts.knownDirectSubclasses.toList
    val termName = TermName("_value")
    children.zipWithIndex.map {
      case (x, i) =>
        Field(
          index = i + 1,
          originalName = None,
          varName = termName,
          default = None,
          tpe = x.asClass.toType,
          repTpe = None,
          baseSealedTrait = Some(T),
          isOption = false
        )
    }
  }

  private def getOrAbort[T](x: Option[T], pos: Position, msg: String) = x match {
    case None => c.abort(pos, msg)
    case Some(value) => value
  }

  private def inferUnmarshallerWireType(tpe: c.universe.Type) = {
    //T.typeArgs.map(_.erasure =:= typeOf[Object])
    if (c.typecheck(q"implicitly[zhukov.Unmarshaller.VarintUnmarshaller[$tpe]]", silent = true).tpe != NoType) VarInt
    else if (c.typecheck(q"implicitly[zhukov.Unmarshaller.Fixed32Unmarshaller[$tpe]]", silent = true).tpe != NoType) Fixed32
    else if (c.typecheck(q"implicitly[zhukov.Unmarshaller.Fixed64Unmarshaller[$tpe]]", silent = true).tpe != NoType) Fixed64
    else if (c.typecheck(q"implicitly[zhukov.Unmarshaller.CodedUnmarshaller[$tpe]]", silent = true).tpe != NoType) Coded
    else LengthDelimited
  }

  private def inferMarshallerWireType(tpe: c.universe.Type) = {
    if (c.typecheck(q"implicitly[zhukov.Marshaller.VarintMarshaller[$tpe]]", silent = true).tpe != NoType) VarInt
    else if (c.typecheck(q"implicitly[zhukov.Marshaller.Fixed32Marshaller[$tpe]]", silent = true).tpe != NoType) Fixed32
    else if (c.typecheck(q"implicitly[zhukov.Marshaller.Fixed64Marshaller[$tpe]]", silent = true).tpe != NoType) Fixed64
    else if (c.typecheck(q"implicitly[zhukov.Marshaller.CodedMarshaller[$tpe]]", silent = true).tpe != NoType) Coded
    else LengthDelimited
  }

  private def commonSizeMeter(x: Field): Tree = x match {
    case Field(i, nameOpt, _, _, _, Some(tpe), _, _) =>
      val name = nameOpt.fold[Tree](Ident(TermName("_v")))(s => q"_value.$s")
      inferMarshallerWireType(tpe) match {
        case VarInt | Fixed32 | Fixed64 => // Packed
          q"""
            val _s = implicitly[zhukov.SizeMeter[$tpe]].measureValues($name)
            _size += ${CodedOutputStream.computeTagSize(i)}
            _size += zhukov.protobuf.CodedOutputStream.computeRawVarint32Size(_s)
            _size += _s
          """
        case Coded =>
          q"""
              val _i = $name.iterator
              while (_i.hasNext) {
                _size += ${CodedOutputStream.computeTagSize(i)}
                _size += implicitly[zhukov.SizeMeter[$tpe]].measure(_i.next())
              }
            """
        case LengthDelimited =>
          q"""
              val _i = $name.iterator
              while (_i.hasNext) {
                val _v = _i.next()
                val _s = implicitly[zhukov.SizeMeter[$tpe]].measure(_v)
                _size += ${CodedOutputStream.computeTagSize(i)}
                _size += zhukov.protobuf.CodedOutputStream.computeRawVarint32Size(_s)
                _size += implicitly[zhukov.SizeMeter[$tpe]].measure(_v)
              }
            """
      }
    case Field(i, nameOpt, _, _, tpe, _, _, _) =>
      val name = nameOpt.fold[Tree](Ident(TermName("_v")))(s => q"_value.$s")
      inferMarshallerWireType(tpe) match {
        case LengthDelimited =>
          q"""
            val _s = implicitly[zhukov.SizeMeter[$tpe]].measure($name)
            _size += ${CodedOutputStream.computeTagSize(i)}
            _size += zhukov.protobuf.CodedOutputStream.computeRawVarint32Size(_s)
            _size += _s
          """
        case _ =>
          q"""
            _size += ${CodedOutputStream.computeTagSize(i)}
            _size += implicitly[zhukov.SizeMeter[$tpe]].measure($name)
          """
      }
    case _ =>
      EmptyTree
  }

  private def commonMarshaller(T: Type, x: Field): Tree = x match {
    case Field(i, nameOpt, _, _, _, Some(tpe), _, false) =>
      val name = nameOpt.fold[Tree](Ident(TermName("_v")))(s => q"_value.$s")
      inferMarshallerWireType(tpe) match {
        case VarInt | Fixed32 | Fixed64 => // Packed
          q"""
            _stream.writeTag($i, ${WireFormat.WIRETYPE_LENGTH_DELIMITED})
            _stream.writeRawVarint32(implicitly[zhukov.SizeMeter[$tpe]].measureValues($name))
            val _i = $name.iterator
            while (_i.hasNext)
              implicitly[zhukov.Marshaller[$tpe]].write(_stream, _i.next())
          """
        case Coded =>
          q"""
            val _i = $name.iterator
            while (_i.hasNext) {
              _stream.writeTag($i, ${WireFormat.WIRETYPE_LENGTH_DELIMITED})
              implicitly[zhukov.Marshaller[$tpe]].write(_stream, _i.next())
            }
          """
        case LengthDelimited =>
          q"""
            val _i = $name.iterator
            while (_i.hasNext) {
              val _v = _i.next()
              _stream.writeTag($i, ${WireFormat.WIRETYPE_LENGTH_DELIMITED})
              _stream.writeRawVarint32(implicitly[zhukov.SizeMeter[$tpe]].measure(_v))
              implicitly[zhukov.Marshaller[$tpe]].write(_stream, _v)
            }
          """
      }
    case Field(i, nameOpt, _, _, tpe, maybeRepTpe, _, isOption) =>
      def writer(tpe: Type, name: Tree) = inferMarshallerWireType(tpe) match {
        case LengthDelimited =>
          q"""
            _stream.writeTag($i, ${WireFormat.WIRETYPE_LENGTH_DELIMITED})
            _stream.writeRawVarint32(implicitly[zhukov.SizeMeter[$tpe]].measure($name))
            implicitly[zhukov.Marshaller[$tpe]].write(_stream, $name)
          """
        case wireType =>
          q"""
            _stream.writeTag($i, ${wireType.value})
            implicitly[zhukov.Marshaller[$tpe]].write(_stream, $name)
          """
      }
      val name = nameOpt.fold[Tree](Ident(TermName("_v")))(s => q"_value.$s")
      if (isOption) {
        q"""
          $name match {
            case Some(__extracted) => ${writer(maybeRepTpe.get, Ident(TermName("__extracted")))}
            case None => ()
          }
        """
      } else writer(tpe, name)
    case _ => EmptyTree
  }

  private val mapSymbol = c.typecheck(tq"Map[_, _]")
    .tpe
    .typeSymbol

  private def commonUnmarshaller(T: Type, fields: List[Field]): Tree = {
    val vars = fields.groupBy(_.varName).mapValues(_.head).collect {
      case (name, Field(_, _, _, Some(default), repTpe, Some(tpe), None, false)) =>
        q"var $name = ${repTpe.typeSymbol.companion}.newBuilder[..${tpe.typeArgs}] ++= $default"
      case (name, Field(_, _, _, Some(default), repTpe, Some(_), None, true)) =>
        q"var $name:$repTpe = $default"
      case (name, Field(_, _, _, Some(default), _, None, None, _)) =>
        q"var $name = $default"
      case (name, Field(_, _, _, None, _, None, Some(parent), _)) =>
        q"var $name:$parent = null"
    }
    val cases = fields.flatMap { x =>
      val tpe = x.repTpe.getOrElse(x.tpe)
      val wireType = inferUnmarshallerWireType(tpe)
      val tag = WireFormat.makeTag(x.index, wireType.value)
      val singleRead = q"implicitly[zhukov.Unmarshaller[$tpe]].read(_stream)"
      if (x.repTpe.isEmpty || x.isOption) {
        val read =
          if (x.isOption) q"Some($singleRead)"
          else singleRead
        wireType match {
          case LengthDelimited =>
            List(
              cq"""
                $tag =>
                  val _length = _stream.readRawVarint32()
                  val _oldLimit = _stream.pushLimit(_length)
                  ${x.varName} = $read
                  _stream.checkLastTagWas(0)
                  _stream.popLimit(_oldLimit)
              """
            )
          case _ =>
            List(cq"$tag => ${x.varName} = $read")
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
    val fields = resolveSealedTraitFields(T, ts)
    q"""
      zhukov.Unmarshaller[$T] { (_stream: zhukov.protobuf.CodedInputStream) =>
        ..${commonUnmarshaller(T, fields)}
        _value
      }
    """
  }

  private def resolveCaseClassFields(T: Type, module: Symbol) = {
    //val constructor = module.typeSignature.decl(applyName).asMethod
    //val params = getOrAbort(constructor.paramLists.headOption, constructor.pos, "Case class should have parameters")
    val params = T.decls.toList.collect { case m: MethodSymbol if m.isCaseAccessor => m }
    params.zipWithIndex.map {
      case (param, i) =>
//        val tpe = param.typeSignature
        val NullaryMethodType(tpe) = param.typeSignatureIn(T)
        val iterableType = typeOf[Iterable[_]]
        val default = {
          val v = TermName(s"apply$$default$$${i + 1}") // apply$default$1
          if (!module.typeSignature.decls.exists(_.name == v)) Some(q"zhukov.Default[$tpe].value")
          else Some(q"$module.$v: $tpe")
        }
        Field(
          index = i + 1,
          originalName = Some(param.name.toTermName),
          varName = TermName("_" + param.name.toString),
          default = default,
          tpe = tpe,
          repTpe =
            if (tpe <:< iterableType) { // Map[K, V] <:< Iterable[(K, V)]
              val xs = tpe.typeArgs
              if (xs.length > 1) Some(tpe.baseType(iterableType.typeSymbol).typeArgs.head)
              //if (xs.length > 1) Some(c.typecheck(q"var ___x: (..$xs) = null; ___x").tpe)
              else Some(xs.head)
            } else if (tpe <:< typeOf[Option[_]]) Some(tpe.typeArgs.head)
            else None,
          baseSealedTrait = None,
          isOption = tpe <:< typeOf[Option[_]]
        )
    }
  }

  private def caseClassUnmarshaller(T: Type, module: Symbol): Tree = {
    val fields = resolveCaseClassFields(T, module)
    val applyArgs = fields.collect {
      case Field(_, Some(originalName), varName, _, _, Some(_), _, false) =>
        q"$originalName = $varName.result()"
      case Field(_, Some(originalName), varName, _, _, None, _, false) =>
        q"$originalName = $varName"
      case Field(_, Some(originalName), varName, _, _, _, _, true) =>
        q"$originalName = $varName"
    }
    val applyArgsInvoke =
      if (module.typeSignature.decls.exists(_.name == applyName)) q"$module.apply(..$applyArgs)"
      else {
        Ident(T.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol
          ].sourceModule.asInstanceOf[Symbol]
        )
      }

    q"""
      zhukov.Unmarshaller[$T] { (_stream: zhukov.protobuf.CodedInputStream) =>
        ..${commonUnmarshaller(T, fields)}
        $applyArgsInvoke
      }
    """
  }

  private def onlyCaseClassesAndSealedTraitsSupported(typeName: String) =
    s"Unable to derive $typeName: Zhukov derivation supports only case classes and sealed traits"

  private val applyName = TermName("apply")

  private sealed abstract class WireType(val value: Int)

  private object WireType {
    case object VarInt extends WireType(WireFormat.WIRETYPE_VARINT)
    case object Fixed64 extends WireType(WireFormat.WIRETYPE_FIXED64)
    case object Fixed32 extends WireType(WireFormat.WIRETYPE_FIXED32)
    case object Coded extends WireType(WireFormat.WIRETYPE_LENGTH_DELIMITED)
    case object LengthDelimited extends WireType(WireFormat.WIRETYPE_LENGTH_DELIMITED)
  }

  private final case class Field(index: Int,
                                 originalName: Option[TermName],
                                 varName: TermName,
                                 default: Option[Tree],
                                 tpe: Type,
                                 repTpe: Option[Type],
                                 baseSealedTrait: Option[Type],
                                 isOption: Boolean)

}
