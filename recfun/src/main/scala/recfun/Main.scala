package recfun

import scala.collection.mutable

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
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

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
          if (x == ')' && a == '(')
            balanceAcc(xs, as)
          else if (x == '(' || x == ')')
            balanceAcc(xs, x :: a :: as)
          else balanceAcc(xs, a :: as)
      }
    }
    chars match {
      case Nil => true
      case c :: xs => balanceAcc(chars, List())
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val res: mutable.HashMap[Int, List[List[Int]]] = mutable.HashMap()
    def cambio(m: Int): List[List[Int]] = {
      if (res.isDefinedAt(m)) res(m)
      else if (m == 0) List(Nil)
      else {
        res += m -> {


          for {
            c <- coins if c <= m
            r: List[Int] <- cambio(m - c)
          } yield (c :: r).sorted
        }.distinct
        res(m)
      }
    }
    cambio(money).size
  }
}
