package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("************************")
    println("Parentheses Balancing")
    var testStr = "HEllo (( )) my (freind).".toList
    println(testStr.toString())
    println(balance(testStr))
    testStr = "HEllo (( ) my (freind).".toList
    println(testStr.toString())
    println(balance(testStr))
    testStr = ")HEllo (( ) my (freind).".toList
    println(testStr.toString())
    println(balance(testStr))

    println("************************")
    println("Counting Change")
    println(countChange(4, List(1, 2)))

  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def checkBanlanced(chars: List[Char], chk: Int): Boolean = {
      if (chars.isEmpty) chk == 0
      else if (chars.head == '(') checkBanlanced(chars.tail, chk + 1)
      else if (chars.head == ')') chk > 0 && checkBanlanced(chars.tail, chk - 1)
      else checkBanlanced(chars.tail, chk)
    }

    checkBanlanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else loop(money, coins.tail) + loop(money - coins.head, coins)
    }

    loop(money, coins)
  }

}
