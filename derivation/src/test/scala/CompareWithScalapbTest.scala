import utest._
import zhukov.Unmarshaller
import zhukov.messages._
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
      "Expr" - {
        val l = Expr(Expr.Value.Lit(Lit(2)))
        val r = Expr(Expr.Value.Lit(Lit(4)))
        val message = Expr(Expr.Value.Add(Add(Some(l), Some(r))))
        val message2 = Unmarshaller[Expr2].read(message.toByteArray)
        assert(message2 == Expr2.Add2(Expr2.Lit2(2), Expr2.Lit2(4)))
      }
    }
  }
}