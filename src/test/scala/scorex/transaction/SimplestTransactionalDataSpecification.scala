package scorex.transaction

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class SimplestTransactionalDataSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with TransactionGen {

  property("SimplestTransactionalData serialization") {
    forAll(paymentSeq) { txs: Seq[LagonakiTransaction] =>
      val d = SimplestTransactionalData(txs)
      val deser = SimplestTransactionalData.parseBytes(d.bytes).get
      deser.transactions.length shouldBe txs.length
      deser.bytes shouldBe d.bytes
    }
  }
}