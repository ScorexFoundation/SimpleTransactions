package scorex.transaction.state

import java.nio.ByteBuffer

import org.h2.mvstore.MVStore
import scorex.block.{Block, TransactionalData}
import scorex.crypto.encode.Base58
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

  protected lazy val heightMap = mvs.openMap[String, Long]("height")

  protected val stateMap = mvs.openMap[ByteBuffer, BoxValue]("state")
  protected val lastIds = mvs.openMap[ByteBuffer, ByteBuffer]("lastId")

  override val version: Int = 0

  override def closedBox(boxId: BoxId): Option[Box[PublicKey25519Proposition]] = {
    Option(stateMap.get(boxId)).flatMap(v => PublicKey25519NoncedBox.parseBytes(v).toOption)
  }

  override def rollbackTo(height: Int): Try[Unit] = Try(mvs.rollbackTo(height))

  override def processBlock(block: Block[PublicKey25519Proposition, _ <: TData, _],
                            fees: Map[PublicKey25519Proposition, Long]): Try[Unit] = Try {
    processTransactions(block.transactionalData.asInstanceOf[SimplestTransactionalData].transactions, fees).get
  }

  def processTransactions(txs: Seq[LagonakiTransaction], fees: Map[PublicKey25519Proposition, Long],
                          checkNegative: Boolean = true): Try[Unit] = Try {
    val currentBoxes = txs.flatMap(tx => Seq(accountBox(tx.sender), accountBox(tx.recipient)).flatten)

    currentBoxes.foreach { b =>
      stateMap.remove(b.id)
    }
    txs.foreach { tx =>
      require(tx.sender.address != tx.recipient.address, "TODO catch")
      val senderBox = currentBoxes.find(_.proposition.address == tx.sender.address) match {
        case Some(b) if !checkNegative || b.value >= tx.amount + tx.fee =>
          b.copy(value = b.value - tx.amount - tx.fee, nonce = tx.txnonce)
        case None if !checkNegative => PublicKey25519NoncedBox(tx.sender, -tx.amount - tx.fee)
        case _ => throw new Error(s"Sender ${tx.sender.address} balance is negative")
      }

      val recepientBox = currentBoxes.find(_.proposition.address == tx.recipient.address)
        .map(b => b.copy(value = b.value + tx.amount, nonce = b.nonce + 1))
        .getOrElse(PublicKey25519NoncedBox(tx.recipient, tx.amount))

      saveBox(senderBox)
      saveBox(recepientBox)
    }
    fees.foreach { m =>
      val minerBox = accountBox(m._1).map(b => b.copy(value = b.value + m._2, nonce = b.nonce + 1))
        .getOrElse(PublicKey25519NoncedBox(m._1, m._2))
      saveBox(minerBox)
    }

    mvs.commit()
    mvs.setStoreVersion((incrementHeight % Int.MaxValue).toInt)
  }

  private def saveBox(nb: PublicKey25519NoncedBox): BoxValue = {
    lastIds.put(ByteBuffer.wrap(nb.proposition.publicKey), ByteBuffer.wrap(nb.id))

    stateMap.put(ByteBuffer.wrap(nb.id), nb.bytes)
  }

  private def incrementHeight: Long = synchronized {
    val currentH = Option(heightMap.get("stateHeight")).getOrElse(0L)
    heightMap.put("stateHeight", currentH + 1)
    currentH + 1
  }

  override def balance(p: PublicKey25519Proposition, height: Option[Int]): Long = {
    accountBox(p).map(_.value).getOrElse(0L)
  }

  private def accountBox(p: PublicKey25519Proposition): Option[PublicKey25519NoncedBox] = {
    Option(lastIds.get(ByteBuffer.wrap(p.publicKey))).flatMap(k => Option(stateMap.get(k)))
      .flatMap(v => PublicKey25519NoncedBox.parseBytes(v).toOption)
  }

  //TODO implement or throw away?
  override def balanceWithConfirmations(id: PublicKey25519Proposition, confirmations: Int): Long = ???

}