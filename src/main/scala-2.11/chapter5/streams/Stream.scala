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

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p:A => Boolean):Boolean = this match {
    case Empty => false
    case Cons(h,t) => (p(h())) || t().exists(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B):B = this match {
    case Empty => z
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
  }

  def forAll(p: A => Boolean):Boolean = this match {
    case Cons(h,t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAll_fold(p: A => Boolean):Boolean = foldRight(true)( (x,y) => y && p(x))

  def takeWhile_fold(p: A => Boolean):Stream[A] = foldRight(empty[A])((x,y) => if(p(x)) cons(x, y) else y )

  def map_fold[B](f: A => B):Stream[B] = foldRight(empty[B])((x,y) => cons(f(x), y))

  def filter_fold(p: A => Boolean):Stream[A] = foldRight(empty[A])((h,t) => if(p(h)) cons(h,t) else t )

  def append[B >: A](x: => Stream[B]):Stream[B] = foldRight(x)((h,t) => cons[B](h,t))

  def flatMap_fold[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)

  def isEmpty:Boolean
}

case object Empty extends Stream[Nothing] {
  def isEmpty:Boolean = true
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  def isEmpty:Boolean = false
}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](xs: A*): Stream[A] = {
    if (xs.isEmpty) Empty else cons[A](xs.head, apply(xs.tail: _*))
  }

  def constant[A](x: A): Stream[A] = cons[A](x, constant[A](x))

  def from(n: Int): Stream[Int] = cons[Int](n, from(n + 1))

  def fibs(): Stream[Int] = {
    def fib(fst: Int, snd: Int): Stream[Int] = cons(fst, fib(snd, fst + snd))
    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(x) => cons(x._1, unfold(x._2)(f))
    case None => empty
  }

  def const_1[A](x:A):Stream[A] = unfold(x)( x => Some((x,x)))

  def from_1(n:Int):Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  def fibs_1():Stream[Int] = unfold((0,1))(x => Some((x._1, (x._2, x._1 + x._2))))
}

