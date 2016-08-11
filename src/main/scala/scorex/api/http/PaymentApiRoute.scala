package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import scorex.app.Application
import scorex.transaction.state.PersistentLagonakiState
import scorex.transaction.{LagonakiTransaction, SimpleTransactionModule, Wallet25519Only}
import scorex.transaction.state.wallet.Payment

import scala.util.{Failure, Success}
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.parser.decode
import scorex.settings.Settings


@Path("/payment")
@Api(value = "/payment", description = "Payment operations.", position = 1)
case class PaymentApiRoute(wallet: Wallet25519Only,
                           state: PersistentLagonakiState,
                           settings: Settings)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  override lazy val route = payment

  @ApiOperation(value = "Send payment",
    notes = "Send payment to another wallet",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.Payment",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response or error")
  ))
  def payment: Route = path("payment") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            decode[Payment](body).toOption match {
              case Some(payment) =>
                val txOpt = LagonakiTransaction.create(payment, wallet)(state)
                txOpt match {
                  case Some(tx) =>
                    tx.validate(state) match {
                      case Success(_) =>
                        tx.json

                      case Failure(e) =>
                        Map("error" -> 0.asJson, "message" -> e.getMessage.asJson).asJson
                    }
                  case None =>
                    ApiError.invalidSender
                }

              case _ =>
                ApiError.wrongJson
            }
          }
        }
      }
    }
  }
}