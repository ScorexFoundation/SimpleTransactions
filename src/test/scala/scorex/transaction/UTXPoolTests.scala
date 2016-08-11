package scorex.transaction

import akka.actor.{Actor, ActorSystem}
import akka.testkit.TestActorRef
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.settings.Settings
import scorex.transaction.proof.Signature25519
import scorex.transaction.state.database.LagonakiUnconfirmedTransactionsDatabase
import scorex.transaction.state.{PersistentLagonakiState, SecretGenerator25519}
import scorex.utils.Random

import scala.util.Failure


class UTXPoolTests extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  val mempool = new LagonakiUnconfirmedTransactionsDatabase {
    override val dirNameOpt: Option[String] = None
  }

  property("put/get transactions") {
    forAll(paymentGenerator) { tx: LagonakiTransaction =>
      mempool.put(tx)
      mempool.getById(tx.id).get shouldBe tx
    }
  }
  property("take transactions") {
    forAll(paymentGenerator) { tx: LagonakiTransaction =>
      mempool.put(tx)
    }
    mempool.take(100)._1.toSeq.length shouldBe 100
  }


}
