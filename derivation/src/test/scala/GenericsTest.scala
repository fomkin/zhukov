import utest._
import zhukov.{Default, Marshaller, SizeMeter, Unmarshaller}
import zhukov.Default.auto._
import zhukov.derivation._

object GenericsTest extends TestSuite {

  final case class Foo[X, Y](x: X, y: Y)


  val tests = Tests {
    "Prederived generics" - {
      implicit val fooMarshaller = marshaller[Foo[Int, Int]]
      implicit val fooUnmarshaller = unmarshaller[Foo[Int, Int]]
      implicit val fooSizeMeter = sizeMeter[Foo[Int, Int]]

      val bytes = Marshaller[Foo[Int, Int]].write(Foo(1, 1))
      val result = Unmarshaller[Foo[Int, Int]].read(bytes)
      assert(result == Foo(1, 1))
    }
    "Generic generics" - {
      implicit def fooMarshaller[T1, T2](implicit t1m: Marshaller[T1], t2m: Marshaller[T2], t1s: SizeMeter[T1], t2s: SizeMeter[T2]) = marshaller[Foo[T1, T2]]
      implicit def fooUnmarshaller[T1: Unmarshaller: Default, T2: Unmarshaller: Default] = unmarshaller[Foo[T1, T2]]
      implicit def fooSizeMeter[T1: SizeMeter, T2: SizeMeter] = sizeMeter[Foo[T1, T2]]

      val bytes = Marshaller[Foo[Int, Int]].write(Foo(1, 1))
      val result = Unmarshaller[Foo[Int, Int]].read(bytes)
      assert(result == Foo(1, 1))
    }
  }
}
