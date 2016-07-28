package scorex.transaction.account

import com.google.common.primitives.{Ints, Longs}
import scorex.serialization.BytesParseable
import scorex.transaction.box.proposition.PublicKey25519Proposition
import shapeless.Sized

import scala.util.Try

case class PublicKey25519NoncedBox(
                                    override val proposition: PublicKey25519Proposition,
                                    override val nonce: Int,
                                    override val value: Long
                                    ) extends PublicKeyNoncedBox[PublicKey25519Proposition] {
  override lazy val bytes: Array[Byte] =
    proposition.publicKey.unsized ++ Ints.toByteArray(nonce) ++ Longs.toByteArray(value)
}

object PublicKey25519NoncedBox extends BytesParseable[PublicKey25519NoncedBox] {
  def apply(proposition: PublicKey25519Proposition, value: Long): PublicKey25519NoncedBox =
    PublicKey25519NoncedBox(proposition, 0, value)

  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519NoncedBox] = Try {
    val pk = PublicKey25519Proposition(Sized.wrap(bytes.take(32)))
    val nonce = Ints.fromByteArray(bytes.slice(32, 36))
    val value = Longs.fromByteArray(bytes.slice(36, 44))
    PublicKey25519NoncedBox(pk, nonce, value)
  }
}
