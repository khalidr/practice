package tree

object FindNodeInTreeFuture extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.Duration
  import scala.concurrent.{Await, Future}

  trait Node {
    def value: String
    def left: Option[Node]
    def right: Option[Node]

    def findNode(value: String): Future[Option[Node]] = {
      def loop(node: Option[Node]): Future[Option[Node]] =
        node match {
          case Some(n) if n.value == value ⇒ Future.successful(node)
          case Some(n) ⇒
            val left = Future(loop(n.left)).flatten
            val right = Future(loop(n.right)).flatten
            for {
              l ← left
              r ← right
            } yield l.orElse(r)
          case None ⇒ Future.successful(None)
        }
      loop(Some(this))
    }

    def contains(str: String): Future[Boolean] = findNode(str).map(_.nonEmpty)
  }

  case class Tree(value: String, left: Option[Node], right: Option[Node]) extends Node
  case class Leaf(value: String) extends Node {
    def left: Option[Node] = None
    def right: Option[Node] = None
  }

  val tree = Tree(
    value = "",
    left = Some(
      Tree(
        value = "???",
        left = Some(Leaf("")),
        right = Some(Tree(
          value = "abc",
          left = None, right = None
        ))
      )
    ),
    right = None
  )

  println(Await.result(tree.contains("abc"), Duration.Inf))
}
