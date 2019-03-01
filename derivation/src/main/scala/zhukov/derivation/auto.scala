package zhukov.derivation

import zhukov.{Marshaller, SizeMeter, Unmarshaller}

import scala.language.experimental.macros

object auto {

  implicit def marshaller[T](implicit lp: LowPriority[Marshaller[T]]): Marshaller[T] = lp.value

  implicit def unmarshaller[T](implicit lp: LowPriority[Unmarshaller[T]]): Unmarshaller[T] = lp.value

  implicit def sizeMeter[T](implicit lp: LowPriority[SizeMeter[T]]): SizeMeter[T] = lp.value

  implicit def unmarshallerMaterializer[T]: LowPriority[Unmarshaller[T]] =
    macro ZhukovDerivationMacro.unmarshallerLowPriorityImpl[T]

  implicit def marshallerMaterializer[T]: LowPriority[Marshaller[T]] =
    macro ZhukovDerivationMacro.marshallerLowPriorityImpl[T]

  implicit def sizeMeterMaterializer[T]: LowPriority[SizeMeter[T]] =
    macro ZhukovDerivationMacro.sizeMeterLowPriorityImpl[T]
}
