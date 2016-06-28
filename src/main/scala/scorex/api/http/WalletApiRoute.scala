package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import scorex.app.Application
import scorex.crypto.encode.Base58
import io.circe.syntax._


@Path("/wallet")
@Api(value = "/wallet", description = "Wallet-related calls")
case class WalletApiRoute(override val application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  private val wallet = application.transactionModule.wallet

  override lazy val route = root ~ seed

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Export wallet seed", httpMethod = "GET")
  def seed: Route = {
    path("wallet" / "seed") {
      getJsonRoute {
        lazy val seedJs = ("seed" -> Base58.encode(wallet.seed)).asJson
        walletNotExists(wallet).getOrElse(seedJs)
      }

    }
  }

  @Path("/")
  @ApiOperation(value = "Wallet", notes = "Display whether wallet exists or not", httpMethod = "GET")
  def root: Route = {
    path("wallet") {
      getJsonRoute {
        ("exists" -> wallet.exists()).asJson
      }
    }
  }
}
