package scorex.transaction.state

import java.nio.ByteBuffer

import org.h2.mvstore.MVStore
import scorex.block.{Block, TransactionalData}
import scorex.transaction._
import scorex.transaction.account.PublicKey25519NoncedBox
import scorex.transaction.box.Box
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.utils.ScorexLogging

import scala.util.Try


/** Store current balances only, and balances changes within effective balance depth.
  * Store transactions for selected accounts only.
  * If no filename provided, blockchain lives in RAM (intended for tests only).
  *
  * Use apply method of PersistentLagonakiState object to create new instance
  */
trait PersistentLagonakiState extends LagonakiState with ScorexLogging {
  type TData = TransactionalData[LagonakiTransaction]
  type BoxId = Array[Byte]
  type BoxValue = Array[Byte]

  def dirNameOpt: Option[String]

  protected lazy val mvs: MVStore = {
    val b = new MVStore.Builder()
    dirNameOpt.map(_ + "/state.dat").foreach(filename => b.fileName(filename))
    b.autoCommitDisabled()
    b.open()
  }
  mvs.setVersionsToKeep(100)

  protected lazy val heightMap = mvs.openMap[String, Int]("height")

  protected val stateMap = mvs.openMap[ByteBuffer, BoxValue]("state")
  protected val lastIds = mvs.openMap[ByteBuffer, ByteBuffer]("lastId")

  override def closedBox(boxId: BoxId): Option[Box[PublicKey25519Proposition]] = {
    Option(stateMap.get(ByteBuffer.wrap(boxId))).flatMap(v => PublicKey25519NoncedBox.parseBytes(v).toOption)
  }

  override def rollbackTo(height: Int): Try[Unit] = Try(mvs.rollbackTo(height))

  override def processBlock(block: Block[PublicKey25519Proposition, _ <: TData, _],
                            fees: Map[PublicKey25519Proposition, Long]): Try[Unit] = Try {
    processTransactions(block.transactionalData.asInstanceOf[SimplestTransactionalData].transactions, fees).get
  }

  def processTransactions(txs: Seq[LagonakiTransaction], fees: Map[PublicKey25519Proposition, Long],
                          checkNegative: Boolean = true): Try[Unit] = Try {
    txs.foreach { tx =>
      val changes = tx.changes(this).get
      changes.toAppend.foreach { b =>
        saveBox(b)
      }
      changes.toRemove.foreach { b =>
        stateMap.remove(b.id)
      }
    }
    fees.foreach { m =>
      val minerBox = accountBox(m._1).map(b => b.copy(value = b.value + m._2, nonce = b.nonce + 1))
        .getOrElse(PublicKey25519NoncedBox(m._1, m._2))
      saveBox(minerBox)
    }

    mvs.commit()
    mvs.setStoreVersion(incrementVersion)
  }

  private def saveBox(nb: Box[PublicKey25519Proposition]): BoxValue = {
    lastIds.put(ByteBuffer.wrap(nb.proposition.publicKey), ByteBuffer.wrap(nb.id))

    stateMap.put(ByteBuffer.wrap(nb.id), nb.bytes)
  }

  override def version: Int = Option(heightMap.get("stateHeight")).getOrElse(0)

  private def incrementVersion: Int = synchronized {
    val currentH = version
    heightMap.put("stateHeight", version + 1)
    currentH + 1
  }

  override def balance(p: PublicKey25519Proposition, height: Option[Int]): Long = {
    accountBox(p).map(_.value).getOrElse(0L)
  }

  def accountBox(p: PublicKey25519Proposition): Option[PublicKey25519NoncedBox] = {
    Option(lastIds.get(ByteBuffer.wrap(p.publicKey))).flatMap(k => Option(stateMap.get(k)))
      .flatMap(v => PublicKey25519NoncedBox.parseBytes(v).toOption)
  }

  //TODO implement or throw away?
  override def balanceWithConfirmations(id: PublicKey25519Proposition, confirmations: Int): Long = ???

}