import minitest.SimpleTestSuite
import zhukov.{Marshaller, Unmarshaller}
import zhukov.derivation._
import zhukov.Default.auto._
object AdtTest extends SimpleTestSuite {

  sealed trait Foo
  case class A(x: Int, y: Int) extends Foo
  case class B(v: String) extends Foo
  case object C extends Foo

  case class Bar(foo: Foo = A(0, 0), x: Int = 0)
  
  implicit val aum = unmarshaller[A]
  implicit val bum = unmarshaller[B]
  implicit val cum = unmarshaller[C.type]
  implicit val am = marshaller[A]
  implicit val bm = marshaller[B]
  implicit val cm = marshaller[C.type]
  implicit val asm = sizeMeter[A]
  implicit val bsm = sizeMeter[B]
  implicit val csm = sizeMeter[C.type]
  implicit val foosm = sizeMeter[Foo]
  implicit val foom = marshaller[Foo]
  implicit val fooum = unmarshaller[Foo]
  implicit val barsm = sizeMeter[Bar]
  implicit val barm = marshaller[Bar]
  implicit val barum = unmarshaller[Bar]

  test("ADT with case object: Foo.A") {
    val sample = A(1, 1)
    val bytes = Marshaller[Foo].write(sample)
    val result = Unmarshaller[Foo].read(bytes)
    assert(result == sample)
  }

  test("ADT with case object: Foo.B") {
    val sample = B("cow")
    val bytes = Marshaller[Foo].write(sample)
    val result = Unmarshaller[Foo].read(bytes)
    assert(result == sample)
  }

  test("ADT with case object: Foo.C") {
    val bytes = Marshaller[Foo].write(C)
    val result = Unmarshaller[Foo].read(bytes)
    assert(result == C)
  }

  test("ADT as part of another message") {
    val sample = Bar(C, 10)
    val bytes = Marshaller[Bar].write(sample)
    val result = Unmarshaller[Bar].read(bytes)
    assert(result == sample)
  }
}
