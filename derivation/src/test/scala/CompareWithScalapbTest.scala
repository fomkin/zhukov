import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import utest._
import zhukov.derivation.{marshaller, sizeMeter, unmarshaller}
import zhukov.messages._
import zhukov.{Bytes, Marshaller, SizeMeter, Unmarshaller}

object CompareWithScalapbTest extends TestSuite {

  case class SimpleMessage2(myNumber: Int = 0, myString: String = "")

  object SimpleMessage2 {
    implicit val u: Unmarshaller[SimpleMessage2] =
      unmarshaller[SimpleMessage2]
    implicit val m: Marshaller[SimpleMessage2] =
      marshaller[SimpleMessage2]
  }

  case class SimpleMessage3(myNumber: Int = 0, myString: Option[String] = None)

  object SimpleMessage3 {
    implicit val u: Unmarshaller[SimpleMessage3] =
      unmarshaller[SimpleMessage3]
    implicit val m: Marshaller[SimpleMessage3] =
      marshaller[SimpleMessage3]
  }

  case class MessageWithSeq2(myString: String = "", myInts: Seq[Int] = Seq())

  object MessageWithSeq2 {
    implicit val u: Unmarshaller[MessageWithSeq2] =
      unmarshaller[MessageWithSeq2]
    implicit val m: Marshaller[MessageWithSeq2] =
      marshaller[MessageWithSeq2]
  }

  case class MessageWithRepeatedString2(myInt: Int = 0,
                                        myStrings: Seq[String] = Nil)

  object MessageWithRepeatedString2 {
    implicit val u: Unmarshaller[MessageWithRepeatedString2] =
      unmarshaller[MessageWithRepeatedString2]
    implicit val m: Marshaller[MessageWithRepeatedString2] =
      marshaller[MessageWithRepeatedString2]
  }

  case class OtherTypes2(f: Float = 0f, d: Double = 0d, b: Boolean = false)

  object OtherTypes2 {
    implicit val u: Unmarshaller[OtherTypes2] =
      unmarshaller[OtherTypes2]
    implicit val m: Marshaller[OtherTypes2] =
      marshaller[OtherTypes2]
  }

  sealed trait Expr2

  object Expr2 {
    //case object Dummy extends Expr2
    case class Lit2(value: Int = 0) extends Expr2
    case class Add2(lhs: Expr2 = Lit2(), rhs: Expr2 = Lit2()) extends Expr2

    implicit val u1: Unmarshaller[Lit2] =
      unmarshaller[Lit2]
    implicit val u2: Unmarshaller[Add2] =
      unmarshaller[Add2]
    implicit val u3: Unmarshaller[Expr2] =
      unmarshaller[Expr2]

    implicit val m1: Marshaller[Lit2] =
      marshaller[Lit2]
    implicit val m2: Marshaller[Add2] =
      marshaller[Add2]
    implicit val m3: Marshaller[Expr2] =
      marshaller[Expr2]

    implicit val sm1: SizeMeter[Lit2] =
      sizeMeter[Lit2]
    implicit val sm2: SizeMeter[Add2] =
      sizeMeter[Add2]
    implicit val sm3: SizeMeter[Expr2] =
      sizeMeter[Expr2]

  }

  case class WithBytes2(myBytes: Array[Byte] = Array.empty, myLong: Long = 0L)

  object WithBytes2 {
    implicit val u: Unmarshaller[WithBytes2] =
      unmarshaller[WithBytes2]
    implicit val m: Marshaller[WithBytes2] =
      marshaller[WithBytes2]
    implicit val sm: SizeMeter[WithBytes2] =
      sizeMeter[WithBytes2]
  }

  case class WithBytes3(myBytes: ByteString = ByteString.EMPTY, myLong: Long = 0L)

  object WithBytes3 {

    implicit object ByteStringBytes extends Bytes[ByteString] {
      def empty: ByteString = ByteString.EMPTY
      def copyFromArray(bytes: Array[Byte]): ByteString = ByteString.copyFrom(bytes)
      def copyFromArray(bytes: Array[Byte], offset: Long, size: Long): ByteString = ByteString.copyFrom(bytes, offset.toInt, size.toInt)
      def copyToArray(value: ByteString, array: Array[Byte], sourceOffset: Int, targetOffset: Int, length: Int): Unit = value.copyTo(array, sourceOffset, targetOffset, length)
      def wrapArray(bytes: Array[Byte]): ByteString = ByteString.copyFrom(bytes)
      def copyBuffer(buffer: ByteBuffer): ByteString = ByteString.copyFrom(buffer)
      def toArray(bytes: ByteString): Array[Byte] = bytes.toByteArray
      def toBuffer(bytes: ByteString): ByteBuffer = bytes.asReadOnlyByteBuffer()
      def get(bytes: ByteString, i: Long): Int = bytes.byteAt(i.toInt)
      def size(bytes: ByteString): Long = bytes.size().toLong
      def concat(left: ByteString, right: ByteString): ByteString = left.concat(right)
      def slice(value: ByteString, start: Long, end: Long): ByteString = value.substring(start.toInt, end.toInt)
    }

    implicit val u: Unmarshaller[WithBytes3] =
      unmarshaller[WithBytes3]
    implicit val m: Marshaller[WithBytes3] =
      marshaller[WithBytes3]
    implicit val sm: SizeMeter[WithBytes3] =
      sizeMeter[WithBytes3]
  }

  val tests = Tests {
    "Write-Read primitives" - {
      'Long - {
        val bs = Marshaller[Long].write(42L)
        assert(Unmarshaller[Long].read(bs) == 42L)
      }
      'Int - {
        val bs = Marshaller[Int].write(42)
        assert(Unmarshaller[Int].read(bs) == 42)
      }
      'String - {
        val bs = Marshaller[String].write("lol")
        assert(Unmarshaller[String].read(bs) == "lol")
      }
    }
    "Read messages, serialized with ScalaPB, using zhukov.Unmarshaller" - {
      'SimpleMessage - {
        val message = new SimpleMessage(42, "cow")
        val message2 = Unmarshaller[SimpleMessage2].read(message.toByteArray)
        assert(
          message.myNumber == message2.myNumber,
          message.myString == message2.myString
        )
      }
      "SimpleMessage with non-empty option" - {
        val message = new SimpleMessage(42, "cow")
        val message2 = Unmarshaller[SimpleMessage3].read(message.toByteArray)
        assert(
          message.myNumber == message2.myNumber,
          message2.myString.contains(message.myString)
        )
      }
      'WithBytes - {
        val message = WithBytes(ByteString.copyFromUtf8("cow"), 42L)
        val message2 = Unmarshaller[WithBytes2].read(message.toByteArray)
        assert(
          message.myBytes.toByteArray.toList == message2.myBytes.toList,
          message.myLong == message2.myLong
        )
      }
      "WithBytes (with ByteString)" - {
        val message = WithBytes(ByteString.copyFromUtf8("cow"), 42L)
        val message2 = Unmarshaller[WithBytes3].read(message.toByteArray)
        assert(
          message.myBytes == message2.myBytes,
          message.myLong == message2.myLong
        )
      }
      "MessageWithSeq (with multiple elements)" - {
        val message = new MessageWithSeq("cow", Seq(42, 9, 12))
        val message2 = Unmarshaller[MessageWithSeq2].read(message.toByteArray)
        assert(
          message.myString == message2.myString,
          message.myInts == message2.myInts
        )
      }
      "MessageWithSeq (with one element sequence)" - {
        val message = new MessageWithSeq("cow", Seq(5))
        val message2 = Unmarshaller[MessageWithSeq2].read(message.toByteArray)
        assert(
          message.myString == message2.myString,
          message.myInts == message2.myInts
        )
      }
      "MessageWithSeq (with empty sequence)" - {
        val message = new MessageWithSeq("cow", Seq())
        val message2 = Unmarshaller[MessageWithSeq2].read(message.toByteArray)
        assert(
          message.myString == message2.myString,
          message.myInts == message2.myInts
        )
      }
      "MessageWithRepeatedString" - {
        val message = new MessageWithRepeatedString(0, Seq("a", "b", "c"))
        val message2 = Unmarshaller[MessageWithRepeatedString2].read(message.toByteArray)
        assert(
          message.myInt == message2.myInt,
          message.myStrings == message2.myStrings
        )
      }
      "MessageWithRepeatedString (empty sequence)" - {
        val message = new MessageWithRepeatedString(0, Nil)
        val message2 = Unmarshaller[MessageWithRepeatedString2].read(message.toByteArray)
        assert(
          message.myInt == message2.myInt,
          message.myStrings == message2.myStrings
        )
      }
      "Expr" - {
        val l = Expr(Expr.Value.Lit(Lit(2)))
        val r = Expr(Expr.Value.Lit(Lit(4)))
        val message = Expr(Expr.Value.Add(Add(Some(l), Some(r))))
        val message2 = Unmarshaller[Expr2].read(message.toByteArray)
        assert(message2 == Expr2.Add2(Expr2.Lit2(2), Expr2.Lit2(4)))
      }
      "OtherTypes" - {
        val message = new OtherTypes(523f, 621d, false)
        val message2 = Unmarshaller[OtherTypes2].read(message.toByteArray)
        assert(
          message.f == message2.f,
          message.d == message2.d,
          message.b == message2.b
        )
      }
    }
    "Read messages, serialized with zhukov.Marshaller, using ScalaPB" - {
      'SimpleMessage - {
        val message2 = SimpleMessage2(42, "cow")
        val message = SimpleMessage.parseFrom(Marshaller[SimpleMessage2].write(message2))
        assert(
          message.myNumber == message2.myNumber,
          message.myString == message2.myString
        )
      }
      "SimpleMessage with non-empty option" - {
        val message2 = new SimpleMessage3(42, Some("cow"))
        val message = SimpleMessage.parseFrom(Marshaller[SimpleMessage3].write(message2))
        assert(
          message.myNumber == message2.myNumber,
          message2.myString.contains(message.myString)
        )
      }
      "SimpleMessage with empty option" - {
        val message2 = new SimpleMessage3(42, None)
        val message = SimpleMessage.parseFrom(Marshaller[SimpleMessage3].write(message2))
        assert(
          message.myNumber == message2.myNumber,
          message.myString == ""
        )
      }
      'WithBytes - {
        val message2 = WithBytes2("cow".getBytes(StandardCharsets.UTF_8), 42L)
        val message = WithBytes.parseFrom(Marshaller[WithBytes2].write(message2))
        assert(
          message.myBytes.toByteArray.toList == message2.myBytes.toList,
          message.myLong == message2.myLong
        )
      }
      "WithBytes (with ByteString)" - {
        val message2 = WithBytes3(ByteString.copyFromUtf8("cow"), 42L)
        val message = WithBytes.parseFrom(Marshaller[WithBytes3].write(message2))
        assert(
          message.myBytes == message2.myBytes,
          message.myLong == message2.myLong
        )
      }
      "MessageWithSeq (with multiple elements)" - {
        val message2 = new MessageWithSeq2("cow", Seq(42, 9, 12))
        val message = MessageWithSeq.parseFrom(Marshaller[MessageWithSeq2].write(message2))
        assert(
          message.myString == message2.myString,
          message.myInts == message2.myInts
        )
      }
      "MessageWithSeq (with one element sequence)" - {
        val message2 = new MessageWithSeq2("cow", Seq(5))
        val message = MessageWithSeq.parseFrom(Marshaller[MessageWithSeq2].write(message2))
        assert(
          message.myString == message2.myString,
          message.myInts == message2.myInts
        )
      }
      "MessageWithSeq (with empty sequence)" - {
        val message2 = new MessageWithSeq2("cow", Nil)
        val message = MessageWithSeq.parseFrom(Marshaller[MessageWithSeq2].write(message2))
        assert(
          message.myString == message2.myString,
          message.myInts == message2.myInts
        )
      }
      "MessageWithRepeatedString" - {
        val message2 = new MessageWithRepeatedString2(0, Seq("a", "b", "c"))
        val message = MessageWithRepeatedString.parseFrom(Marshaller[MessageWithRepeatedString2].write(message2))
        assert(
          message.myInt == message2.myInt,
          message.myStrings == message2.myStrings
        )
      }
      "MessageWithRepeatedString (empty sequence)" - {
        val message2 = new MessageWithRepeatedString2(0, Nil)
        val message = MessageWithRepeatedString.parseFrom(Marshaller[MessageWithRepeatedString2].write(message2))
        assert(
          message.myInt == message2.myInt,
          message.myStrings == message2.myStrings
        )
      }
      "Expr" - {
        val message2 = Expr2.Add2(Expr2.Lit2(2), Expr2.Lit2(4))
        val message = Expr.parseFrom(Marshaller[Expr2].write(message2))
        assert(message == {
          val l = Expr(Expr.Value.Lit(Lit(2)))
          val r = Expr(Expr.Value.Lit(Lit(4)))
          Expr(Expr.Value.Add(Add(Some(l), Some(r))))
        })
      }
      "OtherTypes" - {
        val message2 = OtherTypes2(523f, 621d, b = true)
        val message = OtherTypes.parseFrom(Marshaller[OtherTypes2].write(message2))
        assert(
          message.f == message2.f,
          message.d == message2.d,
          message.b == message2.b
        )
      }
    }
  }
}