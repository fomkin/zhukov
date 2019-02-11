package zhukov

import zhukov.protobuf.CodedOutputStream

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

  implicit val int: SizeMeter[Int] =
    SizeMeter(CodedOutputStream.computeRawVarint32Size _)

  CodedOutputStream
  implicit val string: SizeMeter[String] =
    SizeMeter(CodedOutputStream.computeStringSizeNoTag _)
}