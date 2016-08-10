package scorex.transaction

import io.circe.Json
import scorex.block.TransactionalData
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.BytesParseable

import scala.util.Try
import io.circe.syntax._

case class SimplestTransactionalData(transactions: Seq[LagonakiTransaction])
  extends TransactionalData[LagonakiTransaction] {

  override val mbTransactions: Option[Traversable[LagonakiTransaction]] = Some(transactions)

  //TODO build authenticated data structure and put rootHash here
  override def id: Array[Byte] = FastCryptographicHash(bytes)

  val TransactionSizeLength = 4

  override lazy val json: Json = transactions.map(_.json).asJson

  override lazy val bytes: Array[Byte] = {
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