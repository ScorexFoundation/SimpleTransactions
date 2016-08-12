package scorex.transaction.state.database

import java.nio.ByteBuffer

import scorex.crypto.authds.storage.MvStoreKvStorage
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._


class LagonakiUnconfirmedTransactionsDatabase(dirNameOpt: Option[String]) extends MemoryPool[LagonakiTransaction]
with ScorexLogging {

  private lazy val storage = UTXStorage(dirNameOpt)

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
    val keys = storage.keySeq(limit)
    val txs = keys.flatMap(storage.get)
    keys.foreach(k => storage.unset(k))
    (txs, this)
  }

  override def take(limit: Int): (Traversable[LagonakiTransaction], MemoryPool[LagonakiTransaction]) = {
    (storage.keySeq(limit).flatMap(storage.get), this)
  }

  override def remove(tx: LagonakiTransaction): Unit = filter(tx)

  override def getById(id: Array[Byte]): Option[LagonakiTransaction] = storage.get(ByteBuffer.wrap(id))
}


object LagonakiUnconfirmedTransactionsDatabase {

  import scala.concurrent.duration._

  val MaxTimeForUnconfirmed = 1.hour
  val MaxTransactionsPerBlock = 100
}

case class UTXStorage(fileNameOpt: Option[String]) extends MvStoreKvStorage[ByteBuffer, LagonakiTransaction] {

  def keySeq(limit: Int): Seq[ByteBuffer] = map.keyList().take(limit)
}