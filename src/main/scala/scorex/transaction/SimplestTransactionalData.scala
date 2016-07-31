package scorex.transaction

import io.circe.Json
import scorex.block.TransactionalData
import scorex.serialization.BytesParseable

import scala.util.Try

case class SimplestTransactionalData(transactions: Seq[LagonakiTransaction])
  extends TransactionalData[LagonakiTransaction] {

  override val mbTransactions: Option[Traversable[LagonakiTransaction]] = Some(transactions)

  val TransactionSizeLength = 4

  override def json: Json = ???

  override def bytes: Array[Byte] = {
    if (transactions.nonEmpty) transactions.foldLeft(Array[Byte]())((a, b) => a ++ arrayWithSize(b.bytes))
    else Array()
  }
}

object SimplestTransactionalData extends BytesParseable[SimplestTransactionalData] {
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
        val txs = parseArraySizes(bytes).map(b => LagonakiTransaction.parseBytes(b).get)
        SimplestTransactionalData(txs)
    }
  }
}