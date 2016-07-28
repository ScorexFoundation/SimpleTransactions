package scorex.transaction

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.account.PublicKey25519NoncedBox
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.utils.Random
import shapeless.Sized

class PublicKey25519NoncedBoxSpecification extends PropSpec
with PropertyChecks
with Matchers
with TransactionGen {


  property("bytes()/parse() roundtrip should preserve a transaction") {
    forAll { value: Long =>
      val seed = Random.randomBytes(32)
      val p = PublicKey25519Proposition(Sized.wrap(seed))
      val box = PublicKey25519NoncedBox(p, value)

      val parsed = PublicKey25519NoncedBox.parseBytes(box.bytes).get
      parsed.value shouldBe value
      parsed.proposition.address shouldBe p.address

    }
  }

}