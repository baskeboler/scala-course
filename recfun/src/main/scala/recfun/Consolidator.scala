package recfun

import scala.collection.mutable


case class MyEvt(number: Int)


class Consolidator(observed: List[BankAccount]) extends mutable.Subscriber[MyEvt, mutable.Publisher[MyEvt]] {
  observed.foreach(f = _.subscribe(this))

  private var total: Int = compute()

  def totalBalance = total

  def notify(pub: BankAccount, event: BankAccount) = {
    total = compute()
  }

  private def compute(): Int =
    observed.map(_.accountBalance).sum

  override def notify(pub: mutable.Publisher[MyEvt], event: MyEvt): Unit = total = compute()
}
