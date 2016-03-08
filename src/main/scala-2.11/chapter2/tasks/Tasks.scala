package chapter2.tasks

object Tasks extends App {
  def isSorted[T](arr:Array[T], f: (T,T) => Boolean):Boolean = {
    def loop(arr:Array[T], index:Int):Boolean = (arr, index) match {
      case (_, _) if (index == arr.length - 2) => true
      case (arr, index) => if (f(arr(index), arr(index+1))) loop(arr,index+1) else false
    }

    loop(arr, 0)
  }


  val comparisonFunction:(Int,Int) => Boolean = (x, y) => x <= y

  println(isSorted(Array(1,2,3,4,5,6,7,8,9), comparisonFunction))
  println(isSorted(Array(1,2,3,4,12,5,6,7,8,9), comparisonFunction))

  def partial1[A,B,C](a:A, f: (A,B)=>C): B => C = b => f(a,b)

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = x => y => f(x,y)

  def uncurry[A,B,C](f: A => (B => C)): (A,B) => C = (a,b) => f(a)(b)

  def compose[A,B,C](f: B=>C, g: A=>B): A => C = a => f(g(a))
}
