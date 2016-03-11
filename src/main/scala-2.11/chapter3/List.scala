package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {

  def sum(xs:List[Int]):Int = xs match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(xs:List[Int]):Int = xs match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x,xs) => x * product(xs)
  }

  def tail[A](xs:List[A]):List[A] = xs match {
    case Nil => Nil
    case Cons(x, Cons(y, tail)) => Cons(y,tail)
  }

  def setHead[A](xs:List[A], head:A):List[A] = xs match {
    case Nil => Cons(head, Nil)
    case Cons(x, xs) => Cons(head, xs)
  }

  def drop[A](l:List[A], n: Int):List[A] = (l,n) match {
    case (_, 0) => l
    case (Cons(x,xs),0) => Cons(x,xs)
    case (Nil, n) => throw new IllegalArgumentException
    case (_, n) if n < 0 => throw new IllegalArgumentException
    case (Cons(x,xs),n) => drop(xs, n-1)
  }

  def append[A](xs:List[A], ys:List[A]):List[A] = xs match {
    case Nil => ys
    case Cons(x,xs) => Cons(x, append(xs,ys))
  }

  def init[A](xs:List[A]):List[A] = xs match {
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x, init(xs))
  }

  //this can be called as dropWhile(xs, (a:Int) => a > 0)
//  def dropWhile[A](xs:List[A], f: A => Boolean ):List[A] = xs match {
//    case Nil => Nil
//    case Cons(x,xs) => if (f(x)) dropWhile(xs, f) else xs
//  }

  //it is inconvinient that we have to write type for parameter a. lets rewrite this in curring form
  // in curried form we can get rid off this explicit type declaration
  // now it can be called as dropWhile(xs, a => a > 0)
  def dropWhile[A](xs:List[A])(f: A => Boolean ):List[A] = xs match {
    case Nil => Nil
    case Cons(x,xs) => if (f(x)) dropWhile(xs)(f) else xs
  }

  def foldRight[A, B](xs:List[A], z:B)(f: (A,B) => B):B = xs match {
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A,B](xs:List[A], z:B)(f: (B,A) => B):B = xs match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
  }

  def _sum(x:Int, y:Int):Int = x + y

  def _product(x:Int, y:Int):Int = x * y

  def __sum(xs:List[Int]):Int = foldLeft(xs,0)(_ + _)

  def __product(xs:List[Int]):Int = foldLeft(xs, 0)(_ * _)

  def __length[A](xs:List[A]):Int = foldLeft(xs, 0)( (x,y) => x + 1)

  def length[A](xs:List[A]):Int = foldRight(xs, 0)( (x,y) => y + 1)

  def reverse[A](xs:List[A]):List[A] = {
    @tailrec
    def loop(xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(x, xs) => loop(xs, Cons(x, acc))
    }

    loop(xs, Nil)
  }

  def map[A,B](xs:List[A])(f: A => B):List[B] = {
    def loop(acc:List[B], initial:List[A]):List[B] = initial match {
      case Nil => acc
      case Cons(x,xs) => loop(Cons(f(x),acc), xs)
    }

    loop(Nil, reverse(xs))
  }

  def filter[A](xs:List[A])(f: A => Boolean):List[A] = {
    def loop(acc:List[A], initial:List[A]):List[A] = initial match {
      case Nil => acc
      case Cons(x,xs) => if (f(x)) loop(acc, xs) else loop(Cons(x,acc), xs)
    }

    loop(Nil, reverse(xs))
  }

  def _filter[A](xs:List[A])(f: A => Boolean):List[A] = flatMap(xs)(x => if (f(x)) List(x) else List())

  def flatMap[A,B](xs:List[A])(f: A => List[B]):List[B] = xs match {
    case Nil => Nil
    case Cons(x,xs) => _append(f(x),flatMap(xs)(f))
  }

  def addContents(xs:List[Int], ys:List[Int]):List[Int] = (xs,ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x,xs), Cons(y,ys)) => Cons((x+y), addContents(xs,ys))
  }

  def zipWith[A](xs:List[A], ys:List[A])(f: (A,A) => A):List[A] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x,xs), Cons(y,ys)) => Cons((f(x,y)), zipWith(xs,ys)(f))
  }

  def convertToString(xs:List[Double]):List[String] = map(xs)(x => x.toString)

  def filterOdd(xs:List[Int]):List[Int] = filter(xs)( x => x % 2 != 0)

  def incAllByOne(xs:List[Int]):List[Int] = map[Int, Int](xs)(x => x + 1)

  def _append[A](xs:List[A], ys:List[A]):List[A] = foldLeft(reverse(xs),ys)((acc,head) => Cons(head,acc))

  def _reverse[A](xs:List[A]) = foldLeft(xs, List[A]())( (acc,h) => Cons(h,acc))

  def apply[A](xs:A*):List[A] = if (xs.isEmpty) Nil else Cons(xs.head, apply(xs.tail:_*))
}




