package chapter3

sealed trait Tree[+A]
case class Leaf[+A](value:A) extends Tree[A]
case class Node[+A](left:Tree[A], right:Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree:Tree[A]):Int = tree match  {
    case Leaf(value) => 1
    case Node(left, right) => size(left) + size(right)
  }

  def max(tree:Tree[Int]):Int = {
    def loop(tree:Tree[Int], max:Int):Int = tree match {
      case Leaf(value) => value.max(max)
      case Node(left, right) => loop(left, max).max(loop(right, max))
    }

    loop(tree, Int.MinValue)
  }


}