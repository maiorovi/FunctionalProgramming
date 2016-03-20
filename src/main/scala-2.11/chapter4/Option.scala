package chapter4

trait Option[+A] {
  def map[B](f: A => B):Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]):Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap_1[B](f: A => Option[B]):Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B):B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]):Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def orElse_1[B >: A](ob: => Option[B]):Option[B] =  this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean):Option[A] = this match {
    case None => None
    case Some(a) => if(f(a)) Some(a) else None
  }

  def filter_1(f: A => Boolean):Option[A] = flatMap( x => if (f(x)) Some(x) else None)

}

object Option {

  def map2[A,B,C](a:Option[A], b:Option[B])(f: (A,B) => C):Option[C] = a flatMap ( aa => b map ( bb => f(aa,bb)))

  def map3[A,B,C,D](a:Option[A], b:Option[B], c:Option[C])(f: (A,B,C) => D):Option[D] = a flatMap( aa => b flatMap (bb => c map(cc => f(aa,bb,cc))))

//  def sequence[A](xs:List[Option[A]]):Option[List[A]] = xs match {
//    case Nil => Some(Nil)
//    case x::xs => map2(x,sequence(xs))((a,b) => a::b)
//  }
  def sequence[A](xs:List[Option[A]]):Option[List[A]] = xs.foldRight[Option[List[A]]](Some(Nil))((a,b) => map2(a,b)(_::_))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

}

case class Some[+A](value:A) extends Option[A]
case object None extends Option[Nothing]
