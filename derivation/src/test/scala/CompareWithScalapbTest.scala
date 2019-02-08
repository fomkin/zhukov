import utest._
import zhukov.Unmarshaller
import zhukov.messages.{MessageWithRepeatedString, MessageWithSeq, SimpleMessage}
import zhukov.derivation.unmarshaller

object CompareWithScalapbTest extends TestSuite {

  case class SimpleMessage2(myNumber: Int = 0, myString: String = "")

  object SimpleMessage2 {
    implicit val u: Unmarshaller[SimpleMessage2] =
      unmarshaller[SimpleMessage2]
  }

  case class MessageWithSeq2(myString: String = "", myInts: Seq[Int] = Seq())

  object MessageWithSeq2 {
    implicit val u: Unmarshaller[MessageWithSeq2] =
      unmarshaller[MessageWithSeq2]
  }

  case class MessageWithRepeatedString2(myInt: Int = 0,
                                        myStrings: Seq[String] = Nil)

  object MessageWithRepeatedString2 {
    implicit val u: Unmarshaller[MessageWithRepeatedString2] =
      unmarshaller[MessageWithRepeatedString2]
  }

  val tests = Tests {
    "Read messages, serialized with ScalaPB, with zhukov.Unmarshaller" - {
      'SimpleMessage - {
        val message = new SimpleMessage(42, "cow")
        val message2 = Unmarshaller[SimpleMessage2].read(message.toByteArray)
        assert(
          message.myNumber == message2.myNumber,
          message.myString == message2.myString
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
    }
  }
}