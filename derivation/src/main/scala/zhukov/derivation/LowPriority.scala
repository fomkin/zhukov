package zhukov.derivation

/**
  * LowPriority for auto derivation
  * @see https://stackoverflow.com/questions/33544212/explain-the-lowpriorityimplicits-pattern-used-in-scala-type-level-programming
  */
final case class LowPriority[T](value: T) extends AnyVal
