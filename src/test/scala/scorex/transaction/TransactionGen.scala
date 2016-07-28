package scorex.transaction

import org.scalacheck.{Arbitrary, Gen}
import scorex.settings.SizedConstants._
import scorex.transaction.state.SecretGenerator25519
import shapeless.Sized

trait TransactionGen {

  val byte32: Gen[Array[Byte]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val sizedBytes: Gen[Sized[Array[Byte], Nat32]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(a => Sized.wrap(a.toArray))

  val paymentGenerator: Gen[LagonakiTransaction] = for {
    senderSeed: Array[Byte] <- Arbitrary.arbitrary[Array[Byte]]
    rcpSeed: Array[Byte] <- Arbitrary.arbitrary[Array[Byte]]
    nonce: Int <- Arbitrary.arbitrary[Int]
    amount: Long <- Arbitrary.arbitrary[Long]
    fee: Long <- Arbitrary.arbitrary[Long]
    timestamp: Long <- Arbitrary.arbitrary[Long]
  } yield
    LagonakiTransaction(SecretGenerator25519.generateKeys(senderSeed),
      SecretGenerator25519.generateKeys(rcpSeed).publicCommitment,
      nonce,
      amount,
      fee,
      timestamp)
}
