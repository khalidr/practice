package stringsplitter

import scala.annotation.tailrec
import scala.util.Try
/*
Take an input string parameter and determine:
For all pairs of digits where there are exactly 3 question
marks between them, do all pairings add up to 10.
 For example: 1???1abc???2 is false
abc5???5???5abc is true
abc2???8fyzadfasdf5???5abcasdf is true
 */
object Main extends App {
  def idx(str: String): Option[Int] = {
    val i = str.indexOf("???")
    if (i >= 0) Some(i) else None
  }
  def pairs(str: String): List[Pair] = {
    @tailrec
    def loop(str: Option[String], pairList: List[Pair] = List.empty): List[Pair] = {
      str match {
        case None ⇒ pairList
        case Some(str) ⇒
          val index = idx(str)
          val leftValue = left(str, index)
          val rightValue = right(str, index)
          loop(stringAfterSeparator(str, index), pairList :+ Pair(leftValue, rightValue))
      }
    }
    loop(Option(str))
  }
  def left(str: String, idx: Option[Int]): Option[Int] = {
    idx match {
      case Some(i) ⇒ Some(str.charAt(i - 1).toString.toInt)
      case _ ⇒ None
    }
  }
  def right(str: String, idx: Option[Int]): Option[Int] = {
    idx match {
      case Some(i) ⇒ Try(str.charAt(i + 3)).toOption.map(_.toString.toInt)
      case _ ⇒ None
    }
  }
  def stringAfterSeparator(str: String, indexOfSeparator: Option[Int]): Option[String] =
    indexOfSeparator.map { i ⇒
      str.drop(i + 3)
    }
  case class Pair(left: Option[Int], right: Option[Int])
  def pairsAllEqualTo10(str: String): Boolean = {
    pairs(str).dropRight(1).foldLeft(true) {case (acc, Pair(left, right)) ⇒
      val is10 = for {
        l ← left
        r ← right
      } yield l + r == 10
      acc && is10.getOrElse(false)
    }
  }
  //abc1???2abcaa2???3
  val str = "abc5???5abcaa2???8"
  //val str = "5???5???5"
  println(pairs(str))
  println(pairsAllEqualTo10(str))
}
