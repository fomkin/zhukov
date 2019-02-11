package zhukov

import scala.language.experimental.macros

package object derivation {
  def unmarshaller[T]: Unmarshaller[T] = macro ZhukovDerivationMacro.unmarshallerImpl[T]
  def marshaller[T]: Marshaller[T] = macro ZhukovDerivationMacro.marshallerImpl[T]
  def sizeMeter[T]: SizeMeter[T] = macro ZhukovDerivationMacro.sizeMeterImpl[T]
}
