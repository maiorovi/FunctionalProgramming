package chapter3

import scala.annotation.tailrec

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

  println(List.foldRight(List(1,2,3,4,5),0)(List._sum))

  println(List(1,2,3,4,5))
  println(List.foldRight(List(1,2,3,4,5), Nil:List[Int])(Cons(_,_)))

  println(List.length(List(1,2,3,4,5,6)))
  println(List.length(List(1,2)))

  // lets try to produce stackoverflow error because our foldright implementation is not tailrecursive

  def constructList(n:Int):List[Int] = {
    @tailrec
   def loop(n:Int, acc:List[Int]):List[Int] = n match {
      case 0 => acc
//      case n => Cons(n, constructList(n - 1))
      case n => loop(n-1, Cons(n,acc))
    }

    loop(n, Nil)
  }

  val xs = constructList(1000000)
  // here we have got stack overflowe error as expected
//  println(List.foldRight(xs,0)(List._sum))
  // lets try tailrecursive implementation of fold left with the same list
  println(List.foldLeft(xs,0)(List._sum))
  // foldLeft implementation is tail recursive so we our big list is reduced to a result
  // while foldRight produce stakoverflow error

  //tail recursive reverse is here
  println(List.reverse(List(1,2,3,4,5)))
  println(List._reverse(List(1,2,3,4,5)))
  //append using foldl
  println(List._append(List(1,2,3,4,5),List(6,7,8,9)))

}
