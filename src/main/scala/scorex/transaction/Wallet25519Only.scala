package scorex.transaction

import scorex.settings.Settings
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.state.SecretGenerator25519
import scorex.transaction.wallet.Wallet


class Wallet25519Only(settings: Settings) extends Wallet[PublicKey25519Proposition, SimpleTransactionModule](settings, SecretGenerator25519)
