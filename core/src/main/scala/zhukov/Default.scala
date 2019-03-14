package zhukov

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom

import scala.languageFeature.higherKinds

@implicitNotFound("Case class parameter should have default value or instance of zhukov.Default[${T}] should be provided")
final case class Default[T](value: T)

object Default {

  def apply[T: Default]: Default[T] = implicitly[Default[T]]

  object auto {
    implicit val int: Default[Int] = Default[Int](0)
    implicit val float: Default[Float] = Default[Float](0f)
    implicit val double: Default[Double] = Default[Double](0d)
    implicit val long: Default[Long] = Default[Long](0L)
    implicit val string: Default[String] = Default[String]("")
    implicit val boolean: Default[Boolean] = Default(false)
    implicit def bytes[T: Bytes]: Default[T] = Default(Bytes[T].empty)
    implicit def option[T]: Default[Option[T]] = Default(None)
    implicit def emptyCanBuildFrom[A, Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Default[Col[A]] =
      Default(cbf().result())
    implicit def map[K, V]: Default[Map[K, V]] = Default(Map.empty[K, V])
  }
}