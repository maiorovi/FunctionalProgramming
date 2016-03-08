package chapter3

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


  def apply[A](xs:A*):List[A] = if (xs.isEmpty) Nil else Cons(xs.head, apply(xs.tail:_*))
}




