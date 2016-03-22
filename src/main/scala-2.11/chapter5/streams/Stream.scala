package chapter5.streams

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

 def toList:List[A] = this match {
    case Empty => Nil
      case Cons(h, t) => h()::t().toList
 }


  def toListTailsRec:List[A] = {
    @tailrec
    def loop(xs:Stream[A], acc:List[A]):List[A] = xs match {
      case Empty => acc
      case Cons(h,t) => loop(t(), h()::acc)
    }

    loop(this, Nil).reverse
  }

  def toListFast:List[A] = {
    val buffer: ListBuffer[A] = ListBuffer[A]()
    @tailrec
    def loop(xs:Stream[A]):List[A] = xs match {
      case Empty => buffer.toList
      case Cons(h,t) => {
        buffer.+=(h())
        loop(t())
      }
    }
    loop(this)
  }

  def take(n:Int):Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _ ) if n == 1 => cons(h(), empty)
  }

  def isEmpty:Boolean

}

case object Empty extends Stream[Nothing] {
  def isEmpty:Boolean = true
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  def isEmpty:Boolean = false
}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]):Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]:Stream[A] = Empty

  def apply[A](xs:A*):Stream[A] = {
    if (xs.isEmpty) Empty else cons[A](xs.head, apply(xs.tail:_*))
  }
}

