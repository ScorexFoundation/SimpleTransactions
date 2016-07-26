package scorex.transaction.state

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

  val dirNameOpt: Option[String]

  protected lazy val mvs: MVStore = {
    val b = new MVStore.Builder()
    dirNameOpt.map(_ + "/state.dat").foreach(filename => b.fileName(filename))
    b.autoCommitDisabled()
    b.open()
  }
  mvs.setVersionsToKeep(100)

  protected lazy val heightMap = mvs.openMap[String, Long]("height")

  protected val stateMap = mvs.openMap[BoxId, BoxValue]("state")
  protected val lastIds = mvs.openMap[Array[Byte], BoxId]("lastId")

  override val version: Int = 0

  override def closedBox(boxId: BoxId): Option[Box[PublicKey25519Proposition]] = {
    Option(stateMap.get(boxId)).flatMap(v => PublicKey25519NoncedBox.parseBytes(v).toOption)
  }

  override def rollbackTo(height: Int): Try[Unit] = Try(mvs.rollbackTo(height))

  override def processBlock(block: Block[PublicKey25519Proposition, _ <: TData, _]): Try[Unit] = Try {
    val transactions = block.transactionalData.asInstanceOf[SimplestTransactionalData].transactions
    val toAdd = transactions.flatMap(_.boxesToAdd)
    val toRemove = transactions.flatMap(_.boxesToRemove)
    toRemove.foreach(b => stateMap.remove(b.id))
    toAdd.foreach { b =>
      lastIds.put(b.proposition.publicKey, b.id)
      stateMap.put(b.id, b.bytes)
    }
    mvs.commit()
    mvs.setStoreVersion((incrementHeight % Int.MaxValue).toInt)
  }

  private def incrementHeight: Long = synchronized {
    val currentH = Option(heightMap.get("stateHeight")).getOrElse(0L)
    heightMap.put("stateHeight", currentH + 1)
    currentH + 1
  }

  override def balance(p: PublicKey25519Proposition, height: Option[Int]): Long = {
    Option(lastIds.get(p.publicKey)).flatMap(k => Option(stateMap.get(k)))
      .flatMap(v => PublicKey25519NoncedBox.parseBytes(v).toOption).map(_.value).getOrElse(0L)
  }

  //TODO implement or throw away?
  override def balanceWithConfirmations(id: PublicKey25519Proposition, confirmations: Int): Long = ???

}