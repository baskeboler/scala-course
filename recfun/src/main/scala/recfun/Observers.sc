import recfun.Main.countChange
import recfun._
import scala.collection.immutable.IndexedSeq
import scala.util.Random
object Observers {
  println("hola")
  val change = countChange(200, List(20, 50, 100))
  val accounts: IndexedSeq[BankAccount] = (1 to 500).map(_ =>new BankAccount)
  val d = new Consolidator(accounts.toList)
  def randomAmount = {
    val r = Random.nextInt(1000)
    r.max(-r)
  }
  def randomDeposit = Deposit(randomAmount)
  def randomWithDrawal = Withdrawal(randomAmount)
  def randomTxn: AbstractTransaction with Product with Serializable =
    if (Random.nextBoolean())
      randomDeposit
  else randomWithDrawal
  accounts.foreach(_.processTransaction(randomDeposit))
  def txnRound = for {
    a <- accounts if a.accountBalance > 0
  b <- accounts if (b != a)
  } {
    val t = math.abs(Random.nextInt(a.accountBalance))
    if (t <= a.accountBalance && t > 0) {
      println(s"Transferring $t from $a to $b")
      a.withdrawal(t)
      b.deposit(t)
    }
  }
  txnRound
  d.totalBalance
  txnRound
  d.totalBalance
  txnRound
  d.totalBalance
  val h = accounts.head.transactionHistory
  h.map(_.date).mkString("\n")
}