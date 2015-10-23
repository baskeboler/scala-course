package recfun

import scala.collection.mutable

/**
 * Created by victor on 10/23/15.
 */
class BankAccount extends mutable.Publisher[MyEvt] {
  private var balance: Int = 0

  def accountBalance: Int = balance

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance = balance + amount
      publish(MyEvt(amount))
    }

  def withdrawal(amount: Int): Unit =
    if (amount > 0 && amount < balance) {
      balance = balance - amount
      publish(MyEvt(amount))
    } else throw new Error("invalid withdrawal amount")

}
