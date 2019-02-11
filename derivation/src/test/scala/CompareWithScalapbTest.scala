import utest._
import zhukov.derivation.{marshaller, unmarshaller, sizeMeter}
import zhukov.messages._
import zhukov.{Marshaller, SizeMeter, Unmarshaller}

object CompareWithScalapbTest extends TestSuite {

  case class SimpleMessage2(myNumber: Int = 0, myString: String = "")

  object SimpleMessage2 {
    implicit val u: Unmarshaller[SimpleMessage2] =
      unmarshaller[SimpleMessage2]
    implicit val m: Marshaller[SimpleMessage2] =
      marshaller[SimpleMessage2]
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

  val tests = Tests {
    "Read messages, serialized with ScalaPB, using zhukov.Unmarshaller" - {
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
      "Expr" - {
        val l = Expr(Expr.Value.Lit(Lit(2)))
        val r = Expr(Expr.Value.Lit(Lit(4)))
        val message = Expr(Expr.Value.Add(Add(Some(l), Some(r))))
        val message2 = Unmarshaller[Expr2].read(message.toByteArray)
        assert(message2 == Expr2.Add2(Expr2.Lit2(2), Expr2.Lit2(4)))
      }
    }
    "Write messages, serialized with zhukov.Unmarshaller, using ScalaPB" - {
      'SimpleMessage - {
        val message2 = SimpleMessage2(42, "cow")
        val message = SimpleMessage.parseFrom(Marshaller[SimpleMessage2].write(message2))
        assert(
          message.myNumber == message2.myNumber,
          message.myString == message2.myString
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
    }
  }
}