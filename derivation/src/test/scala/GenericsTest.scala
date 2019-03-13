import minitest.SimpleTestSuite
import zhukov.{Default, Marshaller, SizeMeter, Unmarshaller}
import zhukov.Default.auto._
import zhukov.derivation._

object GenericsTest extends SimpleTestSuite {

  final case class Foo[X, Y](x: X, y: Y)
  final case class Bar(x: Map[String, Int] = Map.empty)

  test("Prederived generics") {
    implicit val fooMarshaller: Marshaller[Foo[Int, Int]] = marshaller[Foo[Int, Int]]
    implicit val fooUnmarshaller: Unmarshaller[Foo[Int, Int]] = unmarshaller[Foo[Int, Int]]
    implicit val fooSizeMeter: SizeMeter[Foo[Int, Int]] = sizeMeter[Foo[Int, Int]]

    val bytes = Marshaller[Foo[Int, Int]].write(Foo(1, 1))
    val result = Unmarshaller[Foo[Int, Int]].read(bytes)
    assert(result == Foo(1, 1))
  }

//    "Generic generics" - {
//      implicit def fooMarshaller[T1, T2](implicit t1m: Marshaller[T1], t2m: Marshaller[T2], t1s: SizeMeter[T1], t2s: SizeMeter[T2]) = marshaller[Foo[T1, T2]]
//      implicit def fooUnmarshaller[T1: Unmarshaller: Default, T2: Unmarshaller: Default] = unmarshaller[Foo[T1, T2]]
//      implicit def fooSizeMeter[T1: SizeMeter, T2: SizeMeter] = sizeMeter[Foo[T1, T2]]
//
//      val bytes = Marshaller[Foo[Int, Int]].write(Foo(1, 1))
//      val result = Unmarshaller[Foo[Int, Int]].read(bytes)
//      assert(result == Foo(1, 1))
//    }

  test("Map (as property of message)") {

    implicit val tplStringIntMarshaller: Marshaller[(String, Int)] = marshaller[(String, Int)]
    implicit val tplStringIntUnmarshaller: Unmarshaller[(String, Int)] = unmarshaller[(String, Int)]
    implicit val tplSm: SizeMeter[(String, Int)] = sizeMeter[(String, Int)]

    implicit val barMarshaller: Marshaller[Bar] = marshaller[Bar]
    implicit val barUnmarshaller: Unmarshaller[Bar] = unmarshaller[Bar]
    implicit val barSm: SizeMeter[Bar] = sizeMeter[Bar]

    val sample = Bar(Map("a" -> 1, "b" -> 2))
    val bytes = Marshaller[Bar].write(sample)
    val result = Unmarshaller[Bar].read(bytes)

    assert(result == sample)
  }
}
