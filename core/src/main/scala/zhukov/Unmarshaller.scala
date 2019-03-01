package zhukov

import zhukov.protobuf.CodedInputStream

sealed trait Unmarshaller[A] {

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
  sealed trait CodedUnmarshaller[A] extends Unmarshaller[A] { self =>
    def map[B](f: A => B): CodedUnmarshaller[B] = new CodedUnmarshaller[B] {
      def read(stream: CodedInputStream): B = f(self.read(stream))
    }
  }

  sealed trait VarintUnmarshaller[A] extends Unmarshaller[A] { self =>
    def map[B](f: A => B): VarintUnmarshaller[B] = new VarintUnmarshaller[B] {
      def read(stream: CodedInputStream): B = f(self.read(stream))
    }
  }

  sealed trait Fixed32Unmarshaller[A] extends Unmarshaller[A] { self =>
    def map[B](f: A => B): Fixed32Unmarshaller[B] = new Fixed32Unmarshaller[B] {
      def read(stream: CodedInputStream): B = f(self.read(stream))
    }
  }

  sealed trait Fixed64Unmarshaller[A] extends Unmarshaller[A] { self =>
    def map[B](f: A => B): Fixed64Unmarshaller[B] = new Fixed64Unmarshaller[B] {
      def read(stream: CodedInputStream): B = f(self.read(stream))
    }
  }

  def apply[T](implicit unmarshaller: Unmarshaller[T]): Unmarshaller[T] =
    unmarshaller

  def apply[T](f: CodedInputStream => T): Unmarshaller[T] = new Unmarshaller[T] {
    def read(stream: CodedInputStream): T = f(stream)
  }

  implicit val bool: VarintUnmarshaller[Boolean] = new VarintUnmarshaller[Boolean] {
    def read(stream: CodedInputStream): Boolean = stream.readBool()
  }

  implicit val int: VarintUnmarshaller[Int] = new VarintUnmarshaller[Int] {
    def read(stream: CodedInputStream): Int = stream.readRawVarint32()
  }

  implicit val long: VarintUnmarshaller[Long] = new VarintUnmarshaller[Long] {
    def read(stream: CodedInputStream): Long = stream.readRawVarint64()
  }

  implicit val float: Fixed32Unmarshaller[Float] = new Fixed32Unmarshaller[Float] {
    def read(stream: CodedInputStream): Float = stream.readFloat()
  }

  implicit val double: Fixed64Unmarshaller[Double] = new Fixed64Unmarshaller[Double] {
    def read(stream: CodedInputStream): Double = stream.readDouble()
  }

  implicit val string: CodedUnmarshaller[String] = new CodedUnmarshaller[String] {
    def read(stream: CodedInputStream): String = stream.readString()
  }

  implicit def bytes[T: Bytes]: CodedUnmarshaller[T] = new CodedUnmarshaller[T] {
    def read(stream: CodedInputStream): T = stream.readBytes
  }
}
