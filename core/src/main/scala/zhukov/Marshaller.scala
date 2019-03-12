package zhukov

import zhukov.protobuf.CodedOutputStream

sealed trait Marshaller[T] {

  def write(stream: CodedOutputStream, value: T): Unit

  def write[B](value: T, bufferSize: Int = CodedOutputStream.DEFAULT_BUFFER_SIZE)(implicit bytes: Bytes[B]): B = {
    val buffer = new Array[Byte](bufferSize)
    val stream = CodedOutputStream.newInstance(buffer)
    write(stream, value)
    bytes.copyFromArray(buffer, 0, bufferSize - stream.spaceLeft())
  }
}

object Marshaller {

  def apply[T: Marshaller]: Marshaller[T] =
    implicitly[Marshaller[T]]

  def apply[T](f: (CodedOutputStream, T) => Unit): Marshaller[T] = new Marshaller[T] {
    def write(stream: CodedOutputStream, value: T): Unit = f(stream, value)
  }

  // This types required to select wire type during derivation

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

  // Default instances

  implicit object IntMarshaller extends VarintMarshaller[Int] {
    def write(stream: CodedOutputStream, value: Int): Unit = {
      stream.writeRawVarint32(value)
    }
  }

  implicit object LongMarshaller extends VarintMarshaller[Long] {
    def write(stream: CodedOutputStream, value: Long): Unit = {
      stream.writeRawVarint64(value)
    }
  }

  implicit object FloatMarshaller extends Fixed32Marshaller[Float] {
    def write(stream: CodedOutputStream, value: Float): Unit = {
      stream.writeFixed32NoTag(java.lang.Float.floatToRawIntBits(value))
    }
  }

  implicit object DoubleMarshaller extends Fixed64Marshaller[Double] {
    def write(stream: CodedOutputStream, value: Double): Unit = {
      stream.writeFixed64NoTag(java.lang.Double.doubleToRawLongBits(value))
    }
  }

  implicit object BooleanMarshaller extends VarintMarshaller[Boolean] {
    def write(stream: CodedOutputStream, value: Boolean): Unit = {
      stream.writeBoolNoTag(value)
    }
  }

  implicit object StringMarshaller extends CodedMarshaller[String] {
    def write(stream: CodedOutputStream, value: String): Unit = {
      stream.writeStringNoTag(value)
    }
  }

  implicit def bytesMarshaller[B: Bytes]: CodedMarshaller[B] = new CodedMarshaller[B] {
    def write(stream: CodedOutputStream, value: B): Unit = {
      stream.writeBytesNoTag(value)
    }
  }
}