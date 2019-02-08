package zhukov

import zhukov.protobuf.{CodedOutputStream, WireFormat}

abstract class Marshaller[@specialized T](val wireType: Int) {


  def apply[B: Bytes](stream: CodedOutputStream, value: T): Unit
  def apply[B: Bytes](value: T, bufferSize: Int = CodedOutputStream.DEFAULT_BUFFER_SIZE): B
}

object Marshaller {

  def apply[T](wireType: Int)(f: (CodedOutputStream, T) => Unit): Marshaller[T] = new Marshaller[T](wireType) {
    def apply[B: Bytes](stream: CodedOutputStream, value: T): Unit = f(stream, value)
    def apply[B](value: T, bufferSize: Int = CodedOutputStream.DEFAULT_BUFFER_SIZE)(implicit bytes: Bytes[B]): B = {
      val buffer = new Array[Byte](bufferSize)
      val stream = CodedOutputStream.newInstance(buffer)
      apply(stream, value)
      bytes.copyFromArray(buffer, 0, bufferSize - stream.spaceLeft())
    }
  }

  // Primitives default instances
  implicit val int: Marshaller[Int] = Marshaller(WireFormat.WIRETYPE_VARINT)(_.writeInt32NoTag(_))
  implicit val long: Marshaller[Long] = Marshaller(WireFormat.WIRETYPE_VARINT)(_.writeInt64NoTag(_))
  implicit val float: Marshaller[Float] = Marshaller(WireFormat.WIRETYPE_FIXED32)(_.writeFloatNoTag(_))
  implicit val double: Marshaller[Double] = Marshaller(WireFormat.WIRETYPE_FIXED32)(_.writeDoubleNoTag(_))
  implicit val string: Marshaller[String] = Marshaller(WireFormat.WIRETYPE_LENGTH_DELIMITED)(_.writeStringNoTag(_))

//  // Sequences
//  implicit def iterable[T]: Marshaller[Iterable[T]] =
//    Marshaller(WireFormat.WIRETYPE_LENGTH_DELIMITED) { (stream, value) =>
//
//    }
}