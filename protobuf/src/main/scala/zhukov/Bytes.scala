package zhukov

import java.nio.ByteBuffer

trait Bytes[T] {
  def empty: T
  def copyFromArray(bytes: Array[Byte]): T
  def copyFromArray(bytes: Array[Byte], offset: Long, size: Long): T
  def copyToArray(value: T, array: Array[Byte], sourceOffset: Int, targetOffset: Int, length: Int): Unit
  def wrapArray(bytes: Array[Byte]): T
  def copyBuffer(buffer: ByteBuffer): T
  def toArray(bytes: T): Array[Byte]
  def toBuffer(bytes: T): ByteBuffer
  def get(bytes: T, i: Long): Int
  def size(bytes: T): Long
  def concat(left: T, right: T): T
  def slice(value: T, start: Long, end: Long): T
}

object Bytes {

  def apply[T: Bytes]: Bytes[T] = implicitly[Bytes[T]]

  implicit final class BytesOps[T](bytes: T)(implicit instance: Bytes[T]) {
    def apply(i: Int): Int = apply(i.toLong)
    def apply(i: Long): Int = instance.get(bytes, i)
    def concat(right: T): T = instance.concat(bytes, right)
  }

  implicit object ArrayInstance extends Bytes[Array[Byte]] {
    
    val empty: Array[Byte] =
      Array()

    def copyFromArray(bytes: Array[Byte]): Array[Byte] =
      bytes.clone()

    def copyFromArray(bytes: Array[Byte], offset: Long, size: Long): Array[Byte] =
      bytes.slice(offset.toInt, (offset + size).toInt)

    def copyToArray(value: Array[Byte], array: Array[Byte], sourceOffset: Int, targetOffset: Int, length: Int): Unit =
      System.arraycopy(value, sourceOffset, array, targetOffset, length)

    def wrapArray(bytes: Array[Byte]): Array[Byte] =
      bytes

    def copyBuffer(buffer: ByteBuffer): Array[Byte] =
      buffer.array()

    def toArray(bytes: Array[Byte]): Array[Byte] =
      bytes

    def toBuffer(bytes: Array[Byte]): ByteBuffer =
      ByteBuffer.wrap(bytes)

    def get(bytes: Array[Byte], i: Long): Int =
      bytes(i.toInt)

    def size(bytes: Array[Byte]): Long =
      bytes.length.toLong

    def concat(left: Array[Byte], right: Array[Byte]): Array[Byte] =
      left ++ right

    def slice(value: Array[Byte], start: Long, end: Long): Array[Byte] =
      value.slice(start.toInt, end.toInt)
  }
}