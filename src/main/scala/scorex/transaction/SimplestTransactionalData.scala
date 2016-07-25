package scorex.transaction

import com.google.common.primitives.Ints
import io.circe.Json
import scorex.block.TransactionalData
import scorex.serialization.BytesParsable

import scala.util.Try

case class SimplestTransactionalData(transactions: Seq[LagonakiTransaction])
  extends TransactionalData[LagonakiTransaction] {

  override val mbTransactions: Option[Traversable[LagonakiTransaction]] = Some(transactions)

  val TransactionSizeLength = 4

  override def json: Json = ???

  override def bytes: Array[Byte] = ???
}

object SimplestTransactionalData extends BytesParsable[SimplestTransactionalData] {
  val TransactionSizeLength = 4

  /**
   * In Lagonaki, transaction-related data is just sequence of transactions. No Merkle-tree root of txs / state etc
   *
   * @param bytes - serialized sequence of transaction
   * @return
   */
  def parseBytes(bytes: Array[Byte]): Try[SimplestTransactionalData] = Try {
    bytes.isEmpty match {
      case true => SimplestTransactionalData(Seq())
      case false =>
        val txData = bytes.tail
        val txCount = bytes.head // so 255 txs max
        SimplestTransactionalData((1 to txCount).foldLeft((0: Int, Seq[LagonakiTransaction]())) { case ((pos, txs), _) =>
          val transactionLengthBytes = txData.slice(pos, pos + TransactionSizeLength)
          val transactionLength = Ints.fromByteArray(transactionLengthBytes)
          val transactionBytes = txData.slice(pos + TransactionSizeLength, pos + TransactionSizeLength + transactionLength)
          val transaction = LagonakiTransaction.parseBytes(transactionBytes).get

          (pos + TransactionSizeLength + transactionLength, txs :+ transaction)
        }._2)
    }
  }
}