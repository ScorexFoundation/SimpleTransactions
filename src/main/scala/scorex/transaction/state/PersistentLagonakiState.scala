package scorex.transaction.state

import io.iohk.iodb.ByteArrayWrapper
import org.h2.mvstore.MVStore
import scorex.block.{StateChanges, TransactionalData}
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
class PersistentLagonakiState(dirNameOpt: Option[String]) extends LagonakiState with ScorexLogging {
  type TData = TransactionalData[LagonakiTransaction]
  type BoxId = Array[Byte]
  type BoxValue = Array[Byte]

  protected lazy val mvs: MVStore = {
    val b = new MVStore.Builder()
    dirNameOpt.map(_ + "/state.dat").foreach(filename => b.fileName(filename))
    b.autoCommitDisabled()
    b.open()
  }
  mvs.setVersionsToKeep(100)

  protected lazy val heightMap = mvs.openMap[String, Int]("height")

  protected lazy val stateMap = mvs.openMap[ByteArrayWrapper, BoxValue]("state")
  protected lazy val lastIds = mvs.openMap[ByteArrayWrapper, ByteArrayWrapper]("lastId")

  override def closedBox(boxId: BoxId): Option[Box[PublicKey25519Proposition]] = {
    Option(stateMap.get(ByteArrayWrapper(boxId))).flatMap(v => PublicKey25519NoncedBox.parseBytes(v).toOption)
  }

  override def rollbackTo(height: Int): Try[MinimalState[PublicKey25519Proposition, LagonakiTransaction]] = Try {
    mvs.rollbackTo(height)
    this
  }

  override def applyChanges(changes: StateChanges[PublicKey25519Proposition]):
  Try[MinimalState[PublicKey25519Proposition, LagonakiTransaction]] = Try {
    changes.toRemove.foreach { b =>
      stateMap.remove(b.id)
    }
    changes.toAppend.foreach { b =>
      saveBox(b)
    }
    incrementVersion
    mvs.commit()
    this
  }

  private def saveBox(nb: Box[PublicKey25519Proposition]): BoxValue = {
    lastIds.put(ByteArrayWrapper(nb.proposition.publicKey), ByteArrayWrapper(nb.id))

    stateMap.put(ByteArrayWrapper(nb.id), nb.bytes)
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
    Option(lastIds.get(ByteArrayWrapper(p.publicKey))).flatMap(k => Option(stateMap.get(k)))
      .flatMap(v => PublicKey25519NoncedBox.parseBytes(v).toOption)
  }
}