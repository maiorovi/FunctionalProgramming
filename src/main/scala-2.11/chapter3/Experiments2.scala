package chapter3

object Experiments2 extends App {
  //TODO: not satisfied with generic implementation of this method in terms of generics
  println(List.incAllByOne(List(1,2,3,4,5)))

  //lets test conversion to String
  println(List.convertToString(List(1.0,2.0,3.0,4.0)))

  //
  println(List.filterOdd(List(1,2,3,4,5,6,7,8)))

  // lets test flatmap
  println(List.flatMap(List(1,2,3,4))( x => List(x,x)))

  println(List.addContents(List(1,2,3,4), List(5,6,7,8)))
}
