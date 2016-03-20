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

  def depth[A](tree:Tree[A]):Int = tree match {
    case Leaf(value) => 0
    case Node(left, right) => {
      val ldepth = 1 + depth(left)
      val rdepth = 1 + depth(right)
      if (ldepth > rdepth) ldepth else rdepth
    }
  }

  def map[A,B](tree:Tree[A])(f: A => B):Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Node(left, right) => Node(map(left)(f), map(right)(f))
  }

  // TODO: implement tree folding
//  def fold[B](tree:Tree[B], z:B)(f: (B,B) => B):B = tree match {
//    case Leaf(value) => f(z,value)
//    case Node(left,right) => f(fold(left,z)(f),fold(right,z)(f))
//  }


//  def _size(tree:Tree[Int]):Int = fold(tree,0)((x,y) => y + 1)


 }