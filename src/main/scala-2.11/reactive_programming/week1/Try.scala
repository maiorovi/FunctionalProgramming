package reactive_programming.week1

trait Try[+A] {
  def flatMap[U](f: A => Try[U]):Try[U] = this match {
    case Success(x) => try f(x) catch {case ex:Exception => Failure(ex)}
    case fail:Failure => fail
  }

  def unit[U](expr: => U):Try[U] = Try(expr)

  def map[U](f: A => U ):Try[U] = this match {
    case Success(x) => Try(f(x))
    case fail: Failure => fail
  }
}

case class Success[T](x:T) extends Try[T]
case class Failure(ex:Exception) extends Try[Nothing]

object Try {
  def apply[T](expr: => T):Try[T] = {
    try Success(expr)
    catch { case ex:Exception => Failure(ex)}
  }

}