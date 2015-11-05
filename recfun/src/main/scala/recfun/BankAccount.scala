package recfun

import java.util.Date

import scala.collection.mutable

/**
 * Created by victor on 10/23/15.
 */
abstract class AbstractTransaction {
  private val instant = new Date()

  def date: Date = instant
}

case class Deposit(amount: Integer) extends AbstractTransaction

case class Withdrawal(amount: Integer) extends AbstractTransaction
object BankAccountId {
  private var current = 0
  def nextId: Int = {
    val n = current
    current += 1
    n
  }
}
class BankAccount extends mutable.Publisher[MyEvt] {
  private val balance: frp = 0
  private var history: mutable.MutableList[AbstractTransaction] = mutable.MutableList()
  val id: Int = BankAccountId.nextId
  override def toString = s"Bankaccount(id=$id, balance=$balance)"

  def accountBalance: Int = balance

  def transactionHistory: List[AbstractTransaction] = history.toList

  def processTransaction(txn: AbstractTransaction) =
    txn match {
      case Deposit(n) => deposit(n)
      case Withdrawal(n) => withdrawal(n)
    }

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      history += Deposit(amount)
      balance = balance + amount
      publish(MyEvt(amount))
    }

  def withdrawal(amount: Int): Unit =
    if (amount > 0 && amount < balance) {
      history += Withdrawal(amount)
      balance = balance - amount
      publish(MyEvt(amount))
    } else throw new Error("invalid withdrawal amount")

}
