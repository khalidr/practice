package tree

//Print the contents of a binary tree
object PrintTreeElements extends App {

  trait Node {
    val value: Int
  }

  case class Leaf(value: Int) extends Node
  case class Tree(value: Int, left: Option[Node], right: Option[Node]) extends Node

  def print(node: Node): Unit = {

    node match {
      case Leaf(value) => println(value)
      case Tree(value, left, right) =>
        println(value)

        left.map(print)
        right.map(print)
    }
  }

  val tree = Tree(5, Some(Tree(1, Some(Leaf(2)), Some(Leaf(3)))), Some(Leaf(4)))

  print(tree)
}