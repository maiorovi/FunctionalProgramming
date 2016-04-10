package chapter6

import scala.util.Random

trait RNG {
  def nextInt(n: Int, rng: RNG):(Int, RNG)
}

case class SimpleRng(seed:Long) extends RNG {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a:A):Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B):Rand[B] = rng => {
      val (a, newRng) = s(rng)
      (f(a), newRng)
    }

  def map2[A,B,C](a:Rand[A], b:Rand[B])(f: (A,B) => C):Rand[C] = rng => {
    val (fst, newRng) = a(rng)
    val (snd, sndRng) = b(newRng)

    (f(fst, snd), sndRng)
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  def nonNegativeEven:Rand[Int] = map[Int, Int](nonNegativeInt)(i => i - i % 2)

  def doubleNew:Rand[Double] = map[Int, Double](nextInt)( _ % Integer.MAX_VALUE)

  def nextInt:Rand[Int] = ???

  override def nextInt(n: Int, rng: RNG): (Int, RNG) = {
    val newseed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = new SimpleRng(newseed)
    val n = (newseed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG):(Int, RNG) = {
    val (value, newRng) = nextInt(Integer.MAX_VALUE, rng)
    (if (value < 0) -(value + 1) else value, newRng)
  }

  def double(rng:RNG):(Double, RNG) = {
    val result = nextInt(Integer.MAX_VALUE - 1, rng)

    (result._1.toDouble / Integer.MAX_VALUE.toDouble, result._2)
  }

  def int:Rand[Int] = rng => nextInt(Integer.MAX_VALUE, rng)


  def intDouble(rng:RNG):((Int, Double), RNG) = {
    val (value, newRng) = nextInt(Integer.MAX_VALUE, rng)
    val (doubleValue, latestRng) = double(newRng)
    ((value, doubleValue), latestRng)
  }

  def doubleInt(rng:RNG):((Double, Int), RNG) = {
    val (result, newRng) = intDouble(rng)
    ((result._2, result._1), newRng)
  }

  def double3(rng:RNG):((Double, Double, Double), RNG) = {
    val one = double(rng)
    val two = double(one._2)
    val three = double(two._2)

    ((one._1, two._1, three._1), three._2)
  }

  def count(count:Int)(rng:RNG):(List[Int], RNG) = {
    def loop(count:Int)(acc:List[Int])(rng:RNG):(List[Int], RNG) = if (count > 0) {
      val (rndValue, newRng) = nextInt(Integer.MAX_VALUE, rng)
      loop(count - 1)(rndValue::acc)(newRng)
    } else {
      (acc, rng)
    }

    loop(count)(Nil)(rng)
  }

//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
//    case Nil => ???
//    case x::y::xs => map
//  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (fst, newRng) = f(rng)
    g(fst)(newRng)
  }

  def _map[A,B](r:Rand[A])(f:A => B): Rand[B] = flatMap(r)( a => unit(f(a)))

  def _map2[A,B,C](r0:Rand[A], r1:Rand[B])(f: (A,B) => C): Rand[C] = flatMap(r0)( a => map(r1)( b => f(a,b)))
}



object Demo extends App {
  val rng = new Random()
  val myRng = new SimpleRng(2500)
//  (0 to 100).foreach( _ => println(rng.nextInt()) )
//  println (100.toDouble)
  println(myRng.double(myRng))

  println(myRng.randIntDouble(myRng))
}