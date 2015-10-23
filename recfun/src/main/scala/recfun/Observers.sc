import recfun.Main.countChange
import recfun.{BankAccount, Consolidator}

object Observers {
  println("hola")
  val change = countChange(1000, List(1, 2, 5, 10))
  val a, b, c = new BankAccount
  val d = new Consolidator(List(a, b, c))
  a deposit 25
  b deposit 100
  a withdrawal 20
  d.totalBalance
}