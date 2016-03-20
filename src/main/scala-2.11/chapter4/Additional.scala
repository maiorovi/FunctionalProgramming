package chapter4

object Additional extends App {

  def variance(xs: Seq[Double]):Option[Double] = mean(xs) flatMap (y => mean(xs.map(x => math.pow( x - y, 2))))

  def mean(xs:Seq[Double]):Option[Double] = if (!xs.isEmpty) Some(xs.foldLeft(0d)( (b,x) => b + x ) / xs.size.toDouble) else None

  println(mean(List(1,2,3,4,5,6,12)).orElse(None))

//  println(Some(5).lift((x:Int) => x + 1))

  println(Option.sequence(List(Some(5),Some(4), Some(3))))
  println(Option.sequence(List(Some(5),Some(4), Some(3), None)))
}
