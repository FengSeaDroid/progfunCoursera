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
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], leftParen: Int): Boolean = (chars, leftParen) match {
      case (Nil, leftParen) => leftParen==0
      case (_, leftParen) if (leftParen < 0) => false
      case (x :: xs, leftParen) if (x == '(') => balanceHelper(xs, leftParen + 1)
      case (x :: xs, leftParen) if (x == ')') => balanceHelper(xs, leftParen - 1)
      case (x :: xs, leftParen) => balanceHelper(xs, leftParen)
    }
    balanceHelper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, coins) => 1
    case (money, coins) if (money < 0 || coins == Nil) => 0
    case (money, coins) => countChange(money - coins.head, coins) +
      countChange(money, coins.tail)
  }
}
