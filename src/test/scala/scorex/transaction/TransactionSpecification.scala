package scorex.transaction

import com.google.common.primitives.Longs
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.state.SecretGenerator25519

class TransactionSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with TransactionGen {

  property("transaction signature should be valid in a valid flow") {
    forAll(paymentGenerator) { tx: LagonakiTransaction =>
      val sig = tx.signature

      sig.isValid(tx.sender, tx.messageToSign) shouldBe true
    }
  }

  property("wrong transaction signature should be invalid") {
    forAll(paymentGenerator) { tx: LagonakiTransaction =>
      val sig = tx.signature

      sig.isValid(tx.sender, tx.copy(fixedFee = tx.fee + 1).messageToSign) shouldBe false
      sig.isValid(tx.sender, tx.copy(amount = tx.amount + 1).messageToSign) shouldBe false
      sig.isValid(tx.sender, tx.copy(timestamp = tx.timestamp + 1).messageToSign) shouldBe false
      sig.isValid(tx.sender, tx.copy(txnonce = tx.txnonce + 1).messageToSign) shouldBe false
      //TODO change sender/recepient
    }
  }

  property("transaction fields should be constructed in a right way") {
    forAll { (senderSeed: Array[Byte],
              recipientSeed: Array[Byte],
              nonce: Int,
              time: Long,
              amount: Long,
              fee: Long) =>

      val sender = SecretGenerator25519.generateKeys(senderSeed)
      val recipient = SecretGenerator25519.generateKeys(recipientSeed).publicCommitment

      val tx = LagonakiTransaction(sender, recipient, nonce, amount, fee, time)

      tx.txnonce shouldEqual nonce
      tx.timestamp shouldEqual time
      tx.amount shouldEqual amount
      tx.fee shouldEqual fee
      tx.sender.address shouldEqual sender.address
      tx.recipient shouldEqual recipient
    }
  }

  property("bytes()/parse() roundtrip should preserve a transaction") {
    forAll(paymentGenerator) { tx: LagonakiTransaction =>

      val txAfter = LagonakiTransaction.parseBytes(tx.bytes).get

      txAfter.getClass.shouldBe(tx.getClass)

      tx.timestamp shouldEqual txAfter.timestamp
      tx.sender.address shouldEqual txAfter.sender.address
      tx.recipient.address shouldEqual txAfter.recipient.address
      tx.txnonce shouldEqual txAfter.txnonce
      tx.amount shouldEqual txAfter.amount
      tx.fee shouldEqual txAfter.fee
      tx.signature.signature shouldEqual txAfter.signature.signature

      txAfter.signature.isValid(tx.sender, tx.messageToSign) shouldBe true
      tx.signature.isValid(tx.sender, tx.messageToSign) shouldBe true
      txAfter.signature.isValid(txAfter.sender, tx.messageToSign) shouldBe true
    }
  }

  ignore("GenesisTransaction Signature should be the same") {

  }

  ignore("GenesisTransaction parse from Bytes should work fine") {

  }
}