import scala.annotation.tailrec

object Main extends App {
  import Methods._

  println(fib1(3))


}

//remove duplicates in a string

//1. sort string
//2. loop
// a) if neighbor is the same as current char then remove neighbor. Pass new string method recursively.
      //move to next letter.
// b) if different then move to next letter passing unaltered string recursively

//aabbbcdevf
/*
1. sort string
2. start head. if length of string ==1 then return head
3. if none, return string
4. else
   a) if current char != lastUnique, add to noDuplicates
   b) loop through string until we find the next char != current
   c) if we find it then recurse with substring(n+m, end)
      else return noDuplicates


4. add head to noDuplicates.
5. find first string not equal to head in n+1 to end string
6. if a char is found then recurse with substring n+m to end
7. if no char found then return nodup
 */


object Methods {

  def unique(word:String, isUnique:Boolean = true):Boolean = {

    val firstLetter:Option[Char] = word.headOption
    val restOfWord:String = if(word.isEmpty) ""  else word.tail

    if(!isUnique)
      false
    else
      firstLetter match {
        case None => isUnique

        case Some(letter) =>
          val u = !restOfWord.exists(_ == letter)

          unique(restOfWord, u )

      }
  }

  //println (unique("adf"))


  //reverse c-style string

  //abcdefj

  //1. start at end of string
  //2. add current char to empty string
  //3. move to n-1 string
  //4. done when no more characters left (i.e. current char is empty)

  def reverse(string:String, reversed:StringBuilder = new StringBuilder):String = {

    string.lastOption match {
      case None ⇒ reversed.toString()
      case Some(s) ⇒
        reversed += s
        reverse(string.take(string.length - 1), reversed)
    }
  }

  def removeDuplicates(original:String):String = {


    /*def helper(sorted:String, noDuplicates:StringBuilder = StringBuilder.newBuilder):String = {
      sorted.headOption match {
        case None ⇒ noDuplicates.toString()
        case Some(c) if sorted.length ==1 ⇒ (noDuplicates += c).toString()
        case Some(c) if sorted.length > 1 ⇒ //get neighbour
          val neighbor = sorted.charAt(1)

          if(neighbor == c) //duplicate
            noDuplicates += c
          else


      }
    }*/
    import util.control.Breaks._

    /*def findNextUnique(c:Char, str:String):Int = {
      for(i ← 1 until str.length) {
        val char = str.charAt(i)
        breakable{
          if (char != c)
            break
          else


        }


      }
    }*/
    //ab
    def helper(sorted:String, noDuplicates:StringBuilder = StringBuilder.newBuilder):String = {
      sorted.headOption match {
        case None ⇒ noDuplicates.toString()
        case Some(h) ⇒
          noDuplicates.append(h.toString)
          //next
          val nextUniqueIdx = sorted.substring(1).indexWhere(_ != h)

          if(nextUniqueIdx == -1)
            noDuplicates.toString()
          else
            helper(sorted.substring(nextUniqueIdx + 1), noDuplicates)

      }
    }

    helper(original.sorted)
  }


  def fib(i:Int) = {

    def loop(n2:Int, n1:Int, idx:Int):Int = {
      idx match {
        case _ if idx == i -1 ⇒ n2 + n1
        /*case 0  ⇒ 0
        case 1 ⇒ 1*/
        case n ⇒ loop(n1, n2 + n1, n +1)

      }
    }

    loop(0, 1, 2)
  }


  def fib1(i:Int) = {

    @tailrec
    def loop(n2:Int, n1:Int, idx:Int):Int = {
      idx match {
        case ii if ii < i ⇒ loop(n1, n2 +n1, idx +1)
        case _ ⇒n2 + n1
      }
    }

    i match {
      case 0 ⇒ 0
      case 1 | 2 ⇒ 1
      case n ⇒ loop(1,1,3)
    }


  }

}
