package chapter4.either

trait Either[+E, +A] {
  def map[B](f: A => B):Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]):Either[EE,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, AA >: A](default: => Either[EE,AA]):Either[EE,AA] = this match {
    case Left(_) => default
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE,C] = for { a <- this; b1 <- b } yield f(a,b1)
}

case class Left[+E](e:E) extends Either[E, Nothing]
case class Right[+A](a:A) extends Either[Nothing, A]

object Either {


  def sequence[E,A](xs:List[Either[E,A]]):Either[E, List[A]] = xs match {
    case Nil => Right(Nil)
    case x::xs =>  x.map2(sequence(xs))(_::_)
  }

  def traverse[E,A](xs:List[A])(f: A => Either[E,A]):Either[E, List[A]] = xs match {
    case Nil => Right(Nil)
    case x::xs => f(x).map2(traverse(xs)(f))(_::_)
  }
}


