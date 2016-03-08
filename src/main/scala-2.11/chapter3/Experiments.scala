package chapter3

object Experiments extends App {
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // this one is work
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  println(x)

  println(List.tail(List(1,2,3,4,5)))

  println(List.init(List(1,2,3,4)))
}
