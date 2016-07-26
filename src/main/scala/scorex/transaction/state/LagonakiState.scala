package scorex.transaction.state

import scorex.transaction.LagonakiTransaction
import scorex.transaction.account.{AccountTransactionsHistory, BalanceSheet}
import scorex.transaction.box.proposition.PublicKey25519Proposition

trait LagonakiState extends MinimalState[PublicKey25519Proposition, LagonakiTransaction]
with BalanceSheet[PublicKey25519Proposition]
