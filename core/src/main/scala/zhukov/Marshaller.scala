package zhukov

import java.nio.ByteBuffer

import zhukov.protobuf.{CodedInputStream, CodedOutputStream}

sealed trait Marshaller[@specialized T] {

  def write(stream: CodedOutputStream, value: T): Unit

  def write[B](value: T, bufferSize: Int = CodedOutputStream.DEFAULT_BUFFER_SIZE)(implicit bytes: Bytes[B]): B = {
    val buffer = new Array[Byte](bufferSize)
    val stream = CodedOutputStream.newInstance(buffer)
    write(stream, value)
    bytes.copyFromArray(buffer, 0, bufferSize - stream.spaceLeft())
  }
}

object Marshaller {

  sealed trait VarintMarshaller[A] extends Marshaller[A] { self =>
    def contramap[B](f: B => A): VarintMarshaller[B] = new VarintMarshaller[B] {
      def write(stream: CodedOutputStream, value: B): Unit =
        self.write(stream, f(value))
    }
  }

  sealed trait Fixed64Marshaller[A] extends Marshaller[A] { self =>
    def contramap[B](f: B => A): Fixed64Marshaller[B] = new Fixed64Marshaller[B] {
      def write(stream: CodedOutputStream, value: B): Unit =
        self.write(stream, f(value))
    }
  }

  sealed trait Fixed32Marshaller[A] extends Marshaller[A] { self =>
    def contramap[B](f: B => A): Fixed32Marshaller[B] = new Fixed32Marshaller[B] {
      def write(stream: CodedOutputStream, value: B): Unit =
        self.write(stream, f(value))
    }
  }

  sealed trait CodedMarshaller[A] extends Marshaller[A] { self =>
    def contramap[B](f: B => A): CodedMarshaller[B] = new CodedMarshaller[B] {
      def write(stream: CodedOutputStream, value: B): Unit =
        self.write(stream, f(value))
    }
  }

  def apply[T: Marshaller]: Marshaller[T] =
    implicitly[Marshaller[T]]

  def apply[T](f: (CodedOutputStream, T) => Unit): Marshaller[T] = new Marshaller[T] {
    def write(stream: CodedOutputStream, value: T): Unit = f(stream, value)
  }

  // Primitives default instances

  implicit object IntMarshaller extends VarintMarshaller[Int] {
    def write(stream: CodedOutputStream, value: Int): Unit = {
      stream.writeRawVarint32(value)
    }
  }

  implicit object StringMarshaller extends CodedMarshaller[String] {
    def write(stream: CodedOutputStream, value: String): Unit = {
      stream.writeStringNoTag(value)
    }
  }

}