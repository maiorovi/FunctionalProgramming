package chapter3

object TreeExperiments extends App {
  val tree = Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))

  println(Tree.size(tree))
  println(Tree.max(tree))

  println(Tree.depth(tree))


  println(Tree.map(tree)(x => 2*x))

//  //test
//  println(Tree._size(tree))
}
