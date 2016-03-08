package chapter2.tasks

import scala.annotation.tailrec

object Fibs extends App {

  def fib(n:Int): Int = n match {
    case _ if n == 0 => 0
    case _ if n == 1 => 1
    case n => fib(n-1) + fib(n-2)
  }

  def fib0(n:Int):Int = {
    @tailrec
    def fib_(n:Int, fst:Int, snd:Int):Int = n match {
      case _ if n == 3 => fst+snd
      case n => fib_(n-1, snd, fst+snd)
    }

    fib_(n, 0,1)
  }

  println(fib0(5))
}
