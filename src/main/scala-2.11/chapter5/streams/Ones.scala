package chapter5.streams

object Ones extends App {

  val ones:Stream[Any] = Stream.cons(1, ones)

  println(ones.take(5).toList)

  val twoes:Stream[Int] = Stream.constant(2)

  twoes.take(10).toList

 println(Stream.from(10).take(5).toList)

  val fibs = Stream.fibs().take(10).toList

  println(fibs)

  val ones1 = Stream.const_1(4).take(5).toList
  println(ones1)
}
