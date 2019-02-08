package zhukov

import scala.language.experimental.macros

package object derivation {
  def unmarshaller[T]: Unmarshaller[T] = macro ZhukovDerivationMacro.unmarshallerImpl[T]
}
