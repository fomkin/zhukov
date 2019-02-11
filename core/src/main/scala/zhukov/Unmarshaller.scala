package zhukov

import zhukov.protobuf.CodedInputStream

trait Unmarshaller[A] {

  def read(stream: CodedInputStream): A

  def read[B: Bytes](bytes: B): A = {
    val stream = CodedInputStream.newInstance(bytes)
    read(stream)
  }
}


object Unmarshaller {

  /**
    * For length-delimited unmarshaller with
    * length-reading defined inside CodedInputStream.
    */
  trait CodedUnmarshaller[A] extends Unmarshaller[A] { self =>
    def map[B](f: A => B): CodedUnmarshaller[B] =
      (stream: CodedInputStream) => f(self.read(stream))
  }

  trait VarintUnmarshaller[A] extends Unmarshaller[A] { self =>
    def map[B](f: A => B): VarintUnmarshaller[B] =
      (stream: CodedInputStream) => f(self.read(stream))
  }

  trait Fixed32Unmarshaller[A] extends Unmarshaller[A] { self =>
    def map[B](f: A => B): Fixed32Unmarshaller[B] =
      (stream: CodedInputStream) => f(self.read(stream))
  }

  trait Fixed64Unmarshaller[A] extends Unmarshaller[A] { self =>
    def map[B](f: A => B): Fixed64Unmarshaller[B] =
      (stream: CodedInputStream) => f(self.read(stream))
  }

  def apply[T](implicit unmarshaller: Unmarshaller[T]): Unmarshaller[T] =
    unmarshaller

  implicit val int: VarintUnmarshaller[Int] = _.readRawVarint32()
  implicit val long: VarintUnmarshaller[Long] = _.readRawVarint64()
  implicit val string: CodedUnmarshaller[String] = _.readString()
}
