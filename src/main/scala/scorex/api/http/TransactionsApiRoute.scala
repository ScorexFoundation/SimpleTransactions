package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.generic.auto._
import io.circe.syntax._
import io.swagger.annotations._
import scorex.app.Application
import scorex.settings.Settings
import scorex.transaction.state.database.LagonakiUnconfirmedTransactionsDatabase
import scorex.transaction.{LagonakiTransaction, MemoryPool, SimpleTransactionModule}
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.state.PersistentLagonakiState

import scala.util.Success

@Path("/transactions")
@Api(value = "/transactions", description = "Information about transactions")
case class TransactionsApiRoute(pool: LagonakiUnconfirmedTransactionsDatabase, override val settings: Settings)
                               (implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed // ~ address ~ adressLimit ~ info
    }

  /* todo: reimplement or throw away?

@Path("/address/{address}/limit/{limit}")
@ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
@ApiImplicitParams(Array(
  new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "String", paramType = "path"),
  new ApiImplicitParam(name = "limit", value = "Specified number of records to be returned", required = true, dataType = "Long", paramType = "path")
))
def adressLimit: Route = {
  path("address" / Segment / "limit" / IntNumber) { case (address, limit) =>
    getJsonRoute {
      PublicKey25519Proposition.validPubKey(address) match {
        case Success(pubkey) =>
          transactionalModule
            .accountTransactions(pubkey)
            .takeRight(limit)
            .map(_.json)
            .asJson
        case _ =>
          ApiError.invalidAddress
      }
    }
  }
}

@Path("/address/{address}")
@ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
@ApiImplicitParams(Array(
  new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "String", paramType = "path")
))
def address: Route = {
  path("address" / Segment) { case address =>
    getJsonRoute {
      PublicKey25519Proposition.validPubKey(address) match {
        case Success(pubkey) =>
          transactionalModule
            .accountTransactions(pubkey)
            .map(_.json)
            .asJson
        case _ =>
          ApiError.invalidAddress
      }
    }
  }
}

@Path("/info/{signature}")
@ApiOperation(value = "Info", notes = "Get transaction info", httpMethod = "GET")
@ApiImplicitParams(Array(
  new ApiImplicitParam(name = "signature", value = "transaction signature ", required = true, dataType = "String", paramType = "path")
))
def info: Route = {
  path("info" / Segment) { case encoded =>
    getJsonRoute {
      Base58.decode(encoded) match {
        case Success(sig) =>
          transactionalModule.included(sig, None) match {
            case Some(h) =>
              Try {
                val block = application.consensusModule.asInstanceOf[StoredBlockchain[_, _]].blockAt(h).get
                val tx = block.transactions.filter(_.proof.bytes sameElements sig).head
                tx.json
              }.getOrElse(Map("status" -> "error", "details" -> "Internal error").asJson)
            case None => Map("status" -> "error", "details" -> "Transaction is not in blockchain").asJson
          }
        case _ => Map("status" -> "error", "details" -> "Incorrect signature").asJson
      }
    }
  }
} */

  @Path("/unconfirmed/{limit}")
  @ApiOperation(value = "Unconfirmed", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "limit", value = "limit ", required = true, dataType = "Int", paramType = "path")
  ))
  def unconfirmed: Route = {
    path("unconfirmed" / IntNumber) { limit =>
      getJsonRoute {
        pool.take(limit)._1.map(_.json).asJson
      }
    }
  }
}
