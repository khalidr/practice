package parentheses

object Main extends App {

  // 1 () 2 (()) or ()() ()()() (())() ()(())  ((()))
  //print out combination of valid parenthesis

  def print(number: Int ): Unit = {

    def loop(openLeft: Int, closedLeft:Int, string: String): Unit = {
      if(openLeft == 0 && closedLeft == 0)
        println(string)

      if(openLeft >0 )
        loop(openLeft -1, closedLeft, string + "(" )

      // ican do close when openleft < closedLeft
      if(openLeft < closedLeft)
      //close
        loop(openLeft, closedLeft-1, string + ")")
    }

    loop(number,number, "")
  }

  print(3)
}