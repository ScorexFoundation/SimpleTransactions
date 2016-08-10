package scorex.transaction.state.database

import java.nio.ByteBuffer

import scorex.crypto.authds.storage.KVStorage
import scorex.transaction._
import scorex.utils.ScorexLogging


trait LagonakiUnconfirmedTransactionsDatabase extends MemoryPool[LagonakiTransaction] with ScorexLogging {

  trait KeySeqSupport[K] {
    def keySeq(): Seq[K]
  }
  val storage: KVStorage[ByteBuffer, LagonakiTransaction, _] with KeySeqSupport[ByteBuffer]

  override def put(tx: LagonakiTransaction): MemoryPool[LagonakiTransaction] = {
    storage.set(ByteBuffer.wrap(tx.id), tx)
    this
  }

  override def filter(id: Array[Byte]): MemoryPool[LagonakiTransaction] = {
    storage.unset(ByteBuffer.wrap(id))
    this
  }

  override def filter(tx: LagonakiTransaction): MemoryPool[LagonakiTransaction] = filter(tx.id)

  override def filter(txs: Traversable[LagonakiTransaction]): MemoryPool[LagonakiTransaction] = {
    txs.foldLeft(this: MemoryPool[LagonakiTransaction]) { (s, tx) =>
      s.filter(tx.id)
    }
  }

  override def put(txs: Traversable[LagonakiTransaction]): MemoryPool[LagonakiTransaction] = {
    txs.foldLeft(this: MemoryPool[LagonakiTransaction]) { (s, tx) =>
      s.put(tx)
    }
  }

  override def drain(limit: Int): (Traversable[LagonakiTransaction], MemoryPool[LagonakiTransaction]) = {
    val keys = storage.keySeq().take(limit)
    val txs = keys.flatMap(storage.get)
    keys.foreach(k =>  storage.unset(k))
    (txs, this)
  }

  override def remove(tx: LagonakiTransaction): Unit = filter(tx)

  override def getById(id: Array[Byte]): Option[LagonakiTransaction] = storage.get(ByteBuffer.wrap(id))
}


object LagonakiUnconfirmedTransactionsDatabase {

  import scala.concurrent.duration._

  val MaxTimeForUnconfirmed = 1.hour
  val MaxTransactionsPerBlock = 100
}