package scorex.network

import akka.actor.ActorRef
import scorex.NodeStateHolder
import scorex.network.NetworkController.DataFromPeer
import scorex.network.TransactionalMessagesRepo.TransactionMessageSpec
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.{SimplestTransactionalData, LagonakiTransaction, SimpleTransactionModule}
import scorex.utils.ScorexLogging

/**
  * Synchronizing transactions that are not in blockchain yet
  */
class UnconfirmedPoolSynchronizer(globalState:NodeStateHolder[PublicKey25519Proposition, LagonakiTransaction, SimplestTransactionalData, _],
                                  override val networkControllerRef: ActorRef) extends ViewSynchronizer with ScorexLogging {

  override val messageSpecs = Seq(TransactionMessageSpec)

  override def receive: Receive = {
    case DataFromPeer(msgId, tx: LagonakiTransaction, remote) if msgId == TransactionMessageSpec.messageCode =>
      log.debug(s"Got tx: $tx")
      (tx, tx.validate(globalState.state).isSuccess) match {
        case (ltx: LagonakiTransaction, true) => globalState.mempool.put(tx)
        case (atx, false) => log.error(s"Transaction $atx is not valid")
        case m => log.error(s"Got unexpected transaction: $m")
      }
  }
}