package scorex.transaction

import scorex.block.TransactionalValidator
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.state.MinimalState

object SimpleTransactionValidator extends TransactionalValidator[PublicKey25519Proposition, LagonakiTransaction, SimplestTransactionalData] {

  override def isValid(tData: SimplestTransactionalData, state: MinimalState[PublicKey25519Proposition, LagonakiTransaction]): Boolean = {
    tData.mbTransactions match {
      case Some(transactions: Seq[LagonakiTransaction]) => state.areValid(transactions)
      case _ => false
    }

  }
}
