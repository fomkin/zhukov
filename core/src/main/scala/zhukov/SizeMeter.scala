package zhukov

import zhukov.protobuf.CodedOutputStream
import zhukov.protobuf.CodedOutputStream.{LITTLE_ENDIAN_32_SIZE, LITTLE_ENDIAN_64_SIZE}

import scala.languageFeature.higherKinds

sealed trait SizeMeter[T] { self =>

  def measureValues(values: Iterable[T]): Int = {
    var size = 0
    val iterator = values.iterator
    while (iterator.hasNext)
      size += measure(iterator.next())
    size
  }

  def measure(value: T): Int

  def contramap[U](f: U => T): SizeMeter[U] = new SizeMeter[U] {
    def measure(value: U): Int = self.measure(f(value))
  }
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
  implicit def bytes[B](implicit bytes: Bytes[B]): SizeMeter[B] = SizeMeter(value => {
    val len = bytes.size(value).toInt
    CodedOutputStream.computeRawVarint32Size(len) + len
  })

  implicit def iterable[A, Col[_] <: Iterable[A]](implicit sm: SizeMeter[A]): SizeMeter[Col[A]] =
    SizeMeter(xs => sm.measureValues(xs.toIterable))
}