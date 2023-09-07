package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ((r == 0) || (r == 1))
      1
    else if ((c == 0) || (c == r))
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val count = 0
    _balance(chars, count)
  }

  def _balance(chars: List[Char], count: Int): Boolean = {
    if (chars.isEmpty)
      (count == 0)
    else {
      if (chars.head == '(')
        count = count + 1
      else if (chars.head == ')')
        count = count - 1
      
      if (count < 0)
        false
      else
        _balance(chars.tail, count)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if ((money < 0) || (coins.isEmpty))
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
