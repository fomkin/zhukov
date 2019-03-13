import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import minitest.SimpleTestSuite
import zhukov.messages._
import zhukov.{Marshaller, Unmarshaller}
import zhukov.derivation.auto._

object CompareWithScalapbAutoderivationTest extends SimpleTestSuite {

  case class SimpleMessage2(myNumber: Int = 0, myString: String = "")

  case class SimpleMessage3(myNumber: Int = 0, myString: Option[String] = None)

  case class MessageWithSeq2(myString: String = "", myInts: Seq[Int] = Seq())

  case class MessageWithRepeatedString2(myInt: Int = 0,
                                        myStrings: Seq[String] = Nil)

  case class OtherTypes2(f: Float = 0f, d: Double = 0d, b: Boolean = false)

//  sealed trait Expr2
//
//  object Expr2 {
//    //case object Dummy extends Expr2
//    case class Lit2(value: Int = 0) extends Expr2
//    case class Add2(lhs: Expr2 = Lit2(), rhs: Expr2 = Lit2()) extends Expr2
//  }

  case class WithBytes2(myBytes: Array[Byte] = Array.empty, myLong: Long = 0L)

  test("Read SimpleMessage") {
    val message = new SimpleMessage(42, "cow")
    val message2 = Unmarshaller[SimpleMessage2].read(message.toByteArray)
    assert(message.myNumber == message2.myNumber)
    assert(message.myString == message2.myString)
  }

  test("Read SimpleMessage with non-empty option") {
    val message = new SimpleMessage(42, "cow")
    val message2 = Unmarshaller[SimpleMessage3].read(message.toByteArray)
    assert(message.myNumber == message2.myNumber)
    assert(message2.myString.contains(message.myString))
  }

  test("Read WithBytes") {
    val message = WithBytes(ByteString.copyFromUtf8("cow"), 42L)
    val message2 = Unmarshaller[WithBytes2].read(message.toByteArray)
    assert(message.myBytes.toByteArray.toList == message2.myBytes.toList)
    assert(message.myLong == message2.myLong)
  }

  test("Read MessageWithSeq (with multiple elements)") {
    val message = new MessageWithSeq("cow", Seq(42, 9, 12))
    val message2 = Unmarshaller[MessageWithSeq2].read(message.toByteArray)
    assert(message.myString == message2.myString)
    assert(message.myInts == message2.myInts)
  }

  test("Read MessageWithSeq (with one element sequence)") {
    val message = new MessageWithSeq("cow", Seq(5))
    val message2 = Unmarshaller[MessageWithSeq2].read(message.toByteArray)
    assert(message.myString == message2.myString)
    assert(message.myInts == message2.myInts)
  }

  test("Read MessageWithSeq (with empty sequence)") {
    val message = new MessageWithSeq("cow", Seq())
    val message2 = Unmarshaller[MessageWithSeq2].read(message.toByteArray)
    assert(message.myString == message2.myString)
    assert(message.myInts == message2.myInts)
  }

  test("Read MessageWithRepeatedString") {
    val message = new MessageWithRepeatedString(0, Seq("a", "b", "c"))
    val message2 = Unmarshaller[MessageWithRepeatedString2].read(message.toByteArray)
    assert(message.myInt == message2.myInt)
    assert(message.myStrings == message2.myStrings)
  }

  test("Read MessageWithRepeatedString (empty sequence)") {
    val message = new MessageWithRepeatedString(0, Nil)
    val message2 = Unmarshaller[MessageWithRepeatedString2].read(message.toByteArray)
    assert(message.myInt == message2.myInt)
    assert(message.myStrings == message2.myStrings)
  }
//      "Expr" - {
//        val l = Expr(Expr.Value.Lit(Lit(2)))
//        val r = Expr(Expr.Value.Lit(Lit(4)))
//        val message = Expr(Expr.Value.Add(Add(Some(l), Some(r))))
//        val message2 = Unmarshaller[Expr2].read(message.toByteArray)
//        assert(message2 == Expr2.Add2(Expr2.Lit2(2), Expr2.Lit2(4)))
//      }
  test("Read OtherTypes") {
    val message = new OtherTypes(523f, 621d, false)
    val message2 = Unmarshaller[OtherTypes2].read(message.toByteArray)
    assert(message.f == message2.f)
    assert(message.d == message2.d)
    assert(message.b == message2.b)
  }

  test("Write SimpleMessage") {
    val message2 = SimpleMessage2(42, "cow")
    val message = SimpleMessage.parseFrom(Marshaller[SimpleMessage2].write(message2))
    assert(message.myNumber == message2.myNumber)
    assert(message.myString == message2.myString)
  }

  test("Write SimpleMessage with non-empty option") {
    val message2 = SimpleMessage3(42, Some("cow"))
    val message = SimpleMessage.parseFrom(Marshaller[SimpleMessage3].write(message2))
    assert(message.myNumber == message2.myNumber)
    assert(message2.myString.contains(message.myString))
  }

  test("Write SimpleMessage with empty option") {
    val message2 = SimpleMessage3(42, None)
    val message = SimpleMessage.parseFrom(Marshaller[SimpleMessage3].write(message2))
    assert(message.myNumber == message2.myNumber)
    assert(message.myString == "")
  }

  test("Write WithBytes") {
    val message2 = WithBytes2("cow".getBytes(StandardCharsets.UTF_8), 42L)
    val message = WithBytes.parseFrom(Marshaller[WithBytes2].write(message2))
    assert(message.myBytes.toByteArray.toList == message2.myBytes.toList)
    assert(message.myLong == message2.myLong)
  }

  test("MessageWithSeq (with multiple elements)") {
    val message2 = MessageWithSeq2("cow", Seq(42, 9, 12))
    val message = MessageWithSeq.parseFrom(Marshaller[MessageWithSeq2].write(message2))
    assert(message.myString == message2.myString)
    assert(message.myInts == message2.myInts)
  }

  test("MessageWithSeq (with one element sequence)") {
    val message2 = MessageWithSeq2("cow", Seq(5))
    val message = MessageWithSeq.parseFrom(Marshaller[MessageWithSeq2].write(message2))
    assert(message.myString == message2.myString)
    assert(message.myInts == message2.myInts)
  }

  test("Write MessageWithSeq (with empty sequence)") {
    val message2 = MessageWithSeq2("cow", Nil)
    val message = MessageWithSeq.parseFrom(Marshaller[MessageWithSeq2].write(message2))
    assert(message.myString == message2.myString)
    assert(message.myInts == message2.myInts)
  }

  test("Write MessageWithRepeatedString") {
    val message2 = MessageWithRepeatedString2(0, Seq("a", "b", "c"))
    val message = MessageWithRepeatedString.parseFrom(Marshaller[MessageWithRepeatedString2].write(message2))
    assert(message.myInt == message2.myInt)
    assert(message.myStrings == message2.myStrings)
  }

  test("MessageWithRepeatedString (empty sequence)") {
    val message2 = MessageWithRepeatedString2(0, Nil)
    val message = MessageWithRepeatedString.parseFrom(Marshaller[MessageWithRepeatedString2].write(message2))
    assert(message.myInt == message2.myInt)
    assert(message.myStrings == message2.myStrings)
  }
//      "Expr" - {
//        val message2 = Expr2.Add2(Expr2.Lit2(2), Expr2.Lit2(4))
//        val message = Expr.parseFrom(Marshaller[Expr2].write(message2))
//        assert(message == {
//          val l = Expr(Expr.Value.Lit(Lit(2)))
//          val r = Expr(Expr.Value.Lit(Lit(4)))
//          Expr(Expr.Value.Add(Add(Some(l), Some(r))))
//        })
//      }
  test("Write OtherTypes") {
    val message2 = OtherTypes2(523f, 621d, b = true)
    val message = OtherTypes.parseFrom(Marshaller[OtherTypes2].write(message2))
    assert(message.f == message2.f)
    assert(message.d == message2.d)
    assert(message.b == message2.b)
  }
}