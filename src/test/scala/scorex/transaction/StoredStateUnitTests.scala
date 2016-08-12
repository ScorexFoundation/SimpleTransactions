package scorex.transaction

import akka.actor.{Actor, ActorSystem}
import akka.testkit.TestActorRef
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.settings.Settings
import scorex.transaction.proof.Signature25519
import scorex.transaction.state.{PersistentLagonakiState, SecretGenerator25519}
import scorex.utils.Random

import scala.util.Failure


class StoredStateUnitTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
with PrivateMethodTester with OptionValues with TransactionGen {

  val settings = new Settings {
    override lazy val filename = "settings-test.json"
  }

  val genesisAcc = SecretGenerator25519.generateKeys(Array())
  val genesisPub = genesisAcc.publicCommitment
  implicit val system = ActorSystem("test")
  val nc = TestActorRef(new Actor {
    def receive = {
      case _ =>
    }
  })
  implicit val state = new PersistentLagonakiState(None)

  val GenesisBalance = Long.MaxValue - 100

  property("State change genesis") {
    state.version shouldBe 0
    val genesisSignature = Signature25519(Array.fill(64)(0: Byte))
    val tx = LagonakiTransaction(LagonakiTransaction.GodAccount, genesisPub, 0, GenesisBalance, 0, 0, genesisSignature)
    tx.validate(state) match {
      case Failure(e) => throw e
      case _ =>
    }
    state.applyChanges(tx.changes(state).get).get
    state.balance(genesisPub) shouldBe GenesisBalance
    state.version shouldBe 1
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
      val amount = Math.abs(amountR) % (state.balance(genesisAcc.publicCommitment) / 400)
      val fee = Math.abs(feeR) % (state.balance(genesisAcc.publicCommitment) / 400)
      whenever(fee > 0 && amount > 0 && amount + fee > 0 &&
        recipient.address != genesisAcc.address && state.balance(genesisAcc.publicCommitment) / 2 > (fee + amount)) {

        val tx = LagonakiTransaction.create(genesisAcc, recipient.publicCommitment, amount, fee).get

        tx.validate(state) match {
          case Failure(e) => throw e
          case _ =>
        }

        val oldS = state.balance(tx.sender)
        val oldR = state.balance(tx.recipient)
        state.applyChanges(tx.changes(state).get.copy(minerReward = 0L)).get
        state.balance(tx.sender) shouldBe (oldS - tx.amount - tx.fee)
        state.balance(tx.recipient) shouldBe (oldR + tx.amount)
      }
    }
  }

  property("State changes for transactions to existing address") {
    val recipient = SecretGenerator25519.generateKeys(Random.randomBytes(32))
    forAll { (amountR: Long, feeR: Long) =>
      val amount = Math.abs(amountR) % (state.balance(genesisAcc.publicCommitment) / 400)
      val fee = Math.abs(feeR) % (state.balance(genesisAcc.publicCommitment) / 400)
      whenever(fee > 0 && amount > 0 && amount + fee > 0 &&
        recipient.address != genesisAcc.address && state.balance(genesisAcc.publicCommitment) / 2 > (fee + amount)) {

        val tx = LagonakiTransaction.create(genesisAcc, recipient.publicCommitment, amount, fee).get

        tx.validate(state).isSuccess shouldBe true

        val oldS = state.balance(tx.sender)
        val oldR = state.balance(tx.recipient)
        state.applyChanges(tx.changes(state).get.copy(minerReward = 0L)).get
        state.balance(tx.sender) shouldBe (oldS - tx.amount - tx.fee)
        state.balance(tx.recipient) shouldBe (oldR + tx.amount)
      }
    }
  }


}
