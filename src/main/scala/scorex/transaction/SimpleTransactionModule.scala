package scorex.transaction

import akka.actor.ActorRef
import scorex.settings.Settings
import scorex.transaction.account.PublicKey25519NoncedBox
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.proof.Signature25519
import scorex.transaction.state.database.LagonakiUnconfirmedTransactionsDatabase
import scorex.transaction.state.wallet.Payment
import scorex.transaction.state.{PersistentLagonakiState, PrivateKey25519Holder, SecretGenerator25519}
import scorex.transaction.wallet.Wallet
import scorex.utils._
import shapeless.Sized

import scala.util.Try


class SimpleTransactionModule(override val settings: Settings, override val networkController: ActorRef)
  extends TransactionalModule[PublicKey25519Proposition, LagonakiTransaction, SimplestTransactionalData]
  with LagonakiUnconfirmedTransactionsDatabase
  with PersistentLagonakiState
  with ScorexLogging {

  override type SH = PrivateKey25519Holder

  override type W = Wallet25519Only

  override val generator = SecretGenerator25519

  override val wallet: W = new Wallet25519Only(settings)

  override val dirNameOpt: Option[String] = settings.dataDirOpt

  val InitialBalance = 60000000000L

  override lazy val genesisData: SimplestTransactionalData = {
    val ipoMembers = List(
      //peer 1 accounts
      "jACSbUoHi4eWgNu6vzAnEx583NwmUAVfS",
      "aptcN9CfZouX7apreDB6WG2cJVbkos881",
      "kVVAu6F21Ax2Ugddms4p5uXz4kdZfAp8g",
      //peer 2 accounts
      "mobNC7SHZRUXDi4GrZP9T2F4iLC1ZidmX",
      "ffUTdmFDesA7NLqLaVfUNgQRD2Xn4tNBp",
      "UR2WjoDCW32XAvYuPbyQW3guxMei5HKf1"
    )

    val timestamp = 0L
    val totalBalance = InitialBalance

    val genesisSignature = Signature25519(Array.fill(64)(0: Byte))
    val txs = ipoMembers.map { addr =>
      val recipient = PublicKey25519Proposition(Sized.wrap(Random.randomBytes(32)))
      LagonakiTransaction(LagonakiTransaction.GodAccount, recipient, 0, totalBalance / ipoMembers.length,
        0, timestamp, genesisSignature)
    }

    SimplestTransactionalData(txs)
  }

  override def transactions(blockData: SimplestTransactionalData): Seq[LagonakiTransaction] =
    blockData.transactions

  def createPayment(payment: Payment, wallet: Wallet[PublicKey25519Proposition, SimpleTransactionModule]): Option[LagonakiTransaction] = {
    wallet.correspondingSecret(payment.sender).flatMap { sender: PrivateKey25519Holder =>
      PublicKey25519Proposition.validPubKey(payment.recipient).flatMap { rcp =>
        createPayment(sender, rcp, payment.amount, payment.fee)
      }.toOption
    }
  }

  def createPayment(sender: PrivateKey25519Holder, recipient: PublicKey25519Proposition, amount: Long, fee: Long): Try[LagonakiTransaction] = Try {
    val time = NTP.correctedTime()
    val nonce = closedBox(sender.publicCommitment.id).get.asInstanceOf[PublicKey25519NoncedBox].nonce
    val paymentTx = LagonakiTransaction(sender, recipient, nonce + 1, amount, fee, time)
    if (isValid(paymentTx)) onNewOffchainTransaction(paymentTx)
    paymentTx
  }

  override def isValid(blockData: SimplestTransactionalData): Boolean =
    blockData.mbTransactions match {
      case Some(transactions: Seq[LagonakiTransaction]) =>
        areValid(transactions)
      case _ => false
    }

  override def parseBytes(bytes: Array[Byte]): Try[SimplestTransactionalData] =
    SimplestTransactionalData.parseBytes(bytes)
}