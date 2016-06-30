package scorex.network

import akka.actor.ActorRef
import scorex.network.NetworkController.DataFromPeer
import scorex.network.TransactionalMessagesRepo.TransactionMessageSpec
import scorex.transaction.{LagonakiTransaction, SimpleTransactionModule}
import scorex.utils.ScorexLogging

/**
  * Synchronizing transactions that are not in blockchain yet
  */
class UnconfirmedPoolSynchronizer(transactionModule:SimpleTransactionModule, override val networkControllerRef: ActorRef) extends ViewSynchronizer with ScorexLogging {

  override val messageSpecs = Seq(TransactionMessageSpec)

  override def receive: Receive = {
    case DataFromPeer(msgId, tx: LagonakiTransaction, remote) if msgId == TransactionMessageSpec.messageCode =>
      log.debug(s"Got tx: $tx")
      (tx, tx.validate(transactionModule).isSuccess) match {
        case (ltx: LagonakiTransaction, true) => transactionModule.putIfNew(ltx)
        case (atx, false) => log.error(s"Transaction $atx is not valid")
        case m => log.error(s"Got unexpected transaction: $m")
      }
  }
}