package zhukov.derivation

import macrocompat.bundle
import zhukov.protobuf.WireFormat

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

@bundle
class ZhukovDerivationMacro(val c: blackbox.Context) {

  import c.universe._

  private val applyName = TermName("apply")

  private final case class Field(
    index: Int,
    originalName: TermName,
    varName: TermName,
    defaultValueName: TermName,
    tpe: Type,
    repTpe: Option[Type]
  )

  private def checkIsClass(x: Symbol) = {
    if (x.isClass) x.asClass
    else c.abort(c.enclosingPosition, "Zhukov derivation supported only for case classes and sealed traits")
  }

  private def getOrAbort[T](x: Option[T], pos: Position, msg: String) = x match {
    case None => c.abort(pos, msg)
    case Some(value) => value
  }

  def unmarshallerImpl[T: WeakTypeTag]: Tree = {
    val T = weakTypeTag[T].tpe
    val targetClass = checkIsClass(T.typeSymbol)
    val companion = targetClass.companion
    val constructor = companion.typeSignature.decl(applyName).asMethod
    val params = getOrAbort(constructor.paramLists.headOption, constructor.pos, "Case class should have parameters")
    val fields = params.zipWithIndex.map {
      case (param, i) =>
        val defaultValue = TermName(s"apply$$default$$${i + 1}") // apply$default$1
        val tpe = param.typeSignature
        if (!companion.typeSignature.decls.exists(_.name == defaultValue))
          c.abort(param.pos, "Parameter should have default value")
        Field(
          index = i + 1,
          originalName = param.name.toTermName,
          varName = TermName("_" + param.name.toString),
          defaultValueName = defaultValue,
          tpe = tpe,
          repTpe = if (tpe <:< typeOf[Iterable[_]]) Some(tpe.typeArgs.head) else None
        )
    }

    val vars = fields.map {
      case Field(_, _, name, default, repTpe, Some(tpe)) =>
        q"var $name = ${repTpe.typeSymbol.companion}.newBuilder[$tpe] ++= $companion.$default"
      case Field(_, _, name, default, _, None) =>
        q"var $name = $companion.$default"
    }
    val applyArgs = fields.map { x =>
      x.repTpe match {
        case Some(_) => q"${x.originalName} = ${x.varName}.result()"
        case None => q"${x.originalName} = ${x.varName}"
      }
    }
    val cases = fields.flatMap { x =>
      val tpe = x.repTpe.getOrElse(x.tpe)
      val wireType = {
        if (c.typecheck(q"implicitly[zhukov.Unmarshaller.VarintUnmarshaller[$tpe]]", silent = true).tpe != NoType) WireFormat.WIRETYPE_VARINT
        else if (c.typecheck(q"implicitly[zhukov.Unmarshaller.Fixed32Unmarshaller[$tpe]]", silent = true).tpe != NoType) WireFormat.WIRETYPE_FIXED32
        else if (c.typecheck(q"implicitly[zhukov.Unmarshaller.Fixed64Unmarshaller[$tpe]]", silent = true).tpe != NoType) WireFormat.WIRETYPE_FIXED64
        else WireFormat.WIRETYPE_LENGTH_DELIMITED
      }
      val tag = WireFormat.makeTag(x.index, wireType)
      val singleRead = q"implicitly[zhukov.Unmarshaller[$tpe]].read(_stream)"
      if (x.repTpe.isEmpty) {
        List(cq"$tag => ${x.varName} = $singleRead")
      } else {
        val cases = List(cq"$tag => ${x.varName} += $singleRead")
        val packed = wireType != WireFormat.WIRETYPE_LENGTH_DELIMITED
        if (!packed) cases else {
          val repTag = WireFormat.makeTag(x.index, WireFormat.WIRETYPE_LENGTH_DELIMITED)
          val `case` = cq"""
              $repTag =>
                val _length = _stream.readRawVarint32()
                val _oldLimit = _stream.pushLimit(_length)
                while (_stream.getBytesUntilLimit > 0)
                  ${x.varName} += $singleRead
                _stream.popLimit(_oldLimit)
            """
          `case` :: cases
        }
      }
    }

    q"""
      new zhukov.Unmarshaller.LengthDelimitedUnmarshaller[$T] {
        def read(_stream: zhukov.protobuf.CodedInputStream) = {
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
         $companion.apply(..$applyArgs)
        }
      }
      """
  }
}
