package scorex.transaction

import akka.actor.{Actor, ActorSystem}
import akka.testkit.TestActorRef
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.settings.Settings
import scorex.transaction.proof.Signature25519
import scorex.transaction.state.SecretGenerator25519
import scorex.utils.Random


class StoredStateUnitTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
with PrivateMethodTester with OptionValues with TransactionGen {

  val settings = new Settings {
    override lazy val filename = "settings-test.json"
  }

  val genesisAcc = SecretGenerator25519.generateKeys(Array())
  implicit val system = ActorSystem("test")
  val nc = TestActorRef(new Actor {
    def receive = {
      case _ =>
    }
  })
  val state = new SimpleTransactionModule(settings, nc)
  val GenesisBalance =  Long.MaxValue - 100

  property("State change genesis") {
    val tx = LagonakiTransaction(genesisAcc, genesisAcc.publicCommitment, 0, GenesisBalance, 0, 0L)
    state.processTransactions(Seq(tx), Map()).get
    state.balance(genesisAcc.publicCommitment) shouldBe GenesisBalance
  }
  property("tx validation") {
    val recepient = SecretGenerator25519.generateKeys(Random.randomBytes(32)).publicCommitment
    val tx = LagonakiTransaction(genesisAcc, recepient, 1, Long.MaxValue, 1L, 123L)
    tx.validate(state).isSuccess shouldBe true
    tx.copy(fee = -1).validate(state).isSuccess shouldBe false
    tx.copy(fee = 0).validate(state).isSuccess shouldBe false
    tx.copy(fee = 101).validate(state).isSuccess shouldBe false
    tx.copy(amount = 1, fee = 9223372036854775807L).validate(state).isSuccess shouldBe false
    tx.copy(amount = -1).validate(state).isSuccess shouldBe false
    tx.copy(amount = GenesisBalance + 1).validate(state).isSuccess shouldBe false
    tx.copy(txnonce = 0).validate(state).isSuccess shouldBe false
    tx.copy(txnonce = 2).validate(state).isSuccess shouldBe false
    tx.copy(signature = Signature25519(Random.randomBytes(tx.signature.signature.length))).validate(state).isSuccess shouldBe false
  }

  property("State change sender and recipient balance") {
    forAll { (amountR: Long, feeR: Long, recSeed: Array[Byte]) =>
      val recipient = SecretGenerator25519.generateKeys(recSeed)
      val amount = 1
      val fee = Math.abs(feeR)
      whenever(fee > 0 && amount > 0 && amount + fee > 0 &&
        recipient.address != genesisAcc.address && state.balance(genesisAcc.publicCommitment) / 2 > (fee + amount)) {

        val tx = state.createPayment(genesisAcc, recipient.publicCommitment, amount, fee).get

        tx.validate(state).isSuccess shouldBe true

        val oldS = state.balance(tx.sender)
        val oldR = state.balance(tx.recipient)
        state.processTransactions(Seq(tx), Map()).get
        val newS = state.balance(tx.sender)
        state.balance(tx.sender) shouldBe (oldS - tx.amount - tx.fee)
        state.balance(tx.recipient) shouldBe (oldR + tx.amount)
      }
    }
  }

}
