package scorex.transaction

import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.settings.Settings


class StoredStateUnitTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
with PrivateMethodTester with OptionValues with TransactionGen {

  val settings = new Settings {
    override lazy val filename = "settings-test.json"
  }

  val state = new SimpleTransactionModule(settings, null)
  property("State change sender and recipient balance") {
    forAll(paymentGenerator) { tx: LagonakiTransaction =>
      whenever(tx.sender.address != tx.recipient.address) {
        val oldS = state.balance(tx.sender)
        val oldR = state.balance(tx.recipient)
        state.processTransactions(Seq(tx), Map(), false).get
        val newS = state.balance(tx.sender)
        state.balance(tx.sender) shouldBe (oldS - tx.amount - tx.fee)
        state.balance(tx.recipient) shouldBe (oldR + tx.amount)
      }
    }
  }

}
