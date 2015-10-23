package recfun

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
  def pascal(c: Int, r: Int): Int =
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c, r + 1) + pascal(c + 1, r + 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceAcc(chars: List[Char], acc: List[Char]): Boolean = {
      (chars, acc) match {
        case (Nil, _) => acc.isEmpty
        case (x :: xs, Nil) =>
          if (x == '(')
            balanceAcc(xs, x :: acc)
          else if (x == ')') false
          else balanceAcc(xs, acc)
        case (x :: xs, a :: as) =>
          if (x == '('))
      }
    }
    chars match {
      case Nil => true
      case c :: xs => c match {
        case '(' => xs.dropWhile(_ != ')')
      }
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
