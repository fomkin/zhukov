package zhukov

import zhukov.protobuf.CodedOutputStream
import zhukov.protobuf.CodedOutputStream.{LITTLE_ENDIAN_32_SIZE, LITTLE_ENDIAN_64_SIZE}

sealed trait SizeMeter[T] {

  def measureValues(values: Iterable[T]): Int = {
    var size = 0
    val iterator = values.iterator
    while (iterator.hasNext)
      size += measure(iterator.next())
    size
  }

  def measure(value: T): Int
}

object SizeMeter {

  def apply[T](implicit sm: SizeMeter[T]): SizeMeter[T] = sm

  def apply[T](f: T => Int): SizeMeter[T] = new SizeMeter[T] {
    def measure(value: T): Int = f(value)
  }

  implicit val int: SizeMeter[Int] = SizeMeter(CodedOutputStream.computeRawVarint32Size _)
  implicit val long: SizeMeter[Long] = SizeMeter(CodedOutputStream.computeRawVarint64Size _)
  implicit val float: SizeMeter[Float] = SizeMeter(_ => LITTLE_ENDIAN_32_SIZE)
  implicit val double: SizeMeter[Double] = SizeMeter(_ => LITTLE_ENDIAN_64_SIZE)
  implicit val boolean: SizeMeter[Boolean] = SizeMeter(CodedOutputStream.computeBoolSizeNoTag _)
  implicit val string: SizeMeter[String] = SizeMeter(CodedOutputStream.computeStringSizeNoTag _)
  implicit def bytes[B](implicit bytes: Bytes[B]): SizeMeter[B] = SizeMeter(value => bytes.size(value).toInt)
  implicit def option[T](implicit sm: SizeMeter[T]): SizeMeter[Option[T]] = SizeMeter(xs => sm.measureValues(xs))
}