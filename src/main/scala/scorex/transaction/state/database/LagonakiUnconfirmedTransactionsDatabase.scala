package scorex.transaction.state.database

import akka.actor.ActorRef
import com.google.common.primitives.Longs
import scorex.network.{Broadcast, NetworkController, TransactionalMessagesRepo}
import scorex.network.message.Message
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.{LagonakiTransaction, SimplestTransactionalData, TransactionalModule, UnconfirmedTransactionsDatabase}
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap


trait LagonakiUnconfirmedTransactionsDatabase
  extends UnconfirmedTransactionsDatabase[LagonakiTransaction, SimplestTransactionalData] with ScorexLogging {

  this: TransactionalModule[PublicKey25519Proposition, LagonakiTransaction, SimplestTransactionalData] =>

  import LagonakiUnconfirmedTransactionsDatabase._

  val networkController: ActorRef

  //TODO move to config
  val SizeLimit = 1000

  val transactions = TrieMap[Long, LagonakiTransaction]()

  //using Long instead of Array[Byte] just for performance improvement
  private def key(signature: Array[Byte]): Long = Longs.fromByteArray(signature.take(8))

  private def key(tx: LagonakiTransaction): Long = key(tx.signature.signature)

  override def putIfNew(tx: LagonakiTransaction): Boolean = if (transactions.size < SizeLimit) {
    transactions.putIfAbsent(key(tx), tx).isEmpty
  } else {
    log.warn("Transaction pool size limit is reached")
    false
  }

  override def remove(tx: LagonakiTransaction): Unit = transactions -= key(tx)

  override def all(): Seq[LagonakiTransaction] = transactions.values.toSeq

  override def getById(signature: Array[Byte]): Option[LagonakiTransaction] = transactions.get(key(signature))

  override def packUnconfirmed(): SimplestTransactionalData =
    SimplestTransactionalData(filterValid(all().sortBy(-_.fee).take(MaxTransactionsPerBlock)))


  //todo: check: clear unconfirmed txs on receiving a block
  override def clearFromUnconfirmed(data: SimplestTransactionalData): Unit = {
    data.transactions.foreach(tx => getById(tx.signature.signature) match {
      case Some(unconfirmedTx) => remove(unconfirmedTx)
      case None =>
    })
  }

  override def onNewOffchainTransaction(transaction: LagonakiTransaction): Unit = transaction match {
    case tx: LagonakiTransaction =>
      if (putIfNew(tx)) {
        val spec = TransactionalMessagesRepo.TransactionMessageSpec
        val ntwMsg = Message(spec, Right(tx), None)
        networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
      }
    case _ => throw new Error("Wrong kind of transaction!")
  }
}


object LagonakiUnconfirmedTransactionsDatabase {
  import scala.concurrent.duration._

  val MaxTimeForUnconfirmed = 1.hour
  val MaxTransactionsPerBlock = 100
}