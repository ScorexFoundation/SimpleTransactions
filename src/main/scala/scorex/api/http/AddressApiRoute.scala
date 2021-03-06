package scorex.api.http

import java.nio.charset.StandardCharsets
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import io.swagger.annotations._
import scorex.api.http.ApiError._
import scorex.crypto.encode.Base58
import scorex.settings.Settings
import scorex.transaction.Wallet25519Only
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.state.PersistentLagonakiState
import shapeless.Sized

import scala.util.{Failure, Success, Try}

@Path("/wallet")
@Api(value = "/wallet/", description = "Info about wallet's accounts and other calls about addresses")
case class AddressApiRoute(wallet: Wallet25519Only,
                           state: PersistentLagonakiState,
                           settings: Settings)(implicit val context: ActorRefFactory) extends ApiRoute
with CommonTransactionApiFunctions {


  override lazy val route =
    pathPrefix("addresses") {
      validate ~ seed ~ balance ~ verify ~ sign ~ deleteAddress ~ verifyText ~
        signText ~ seq
    } ~ root ~ create

  @Path("/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the account with address {address} from the wallet", httpMethod = "DELETE")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def deleteAddress: Route = {
    path(Segment) { case address =>
      withAuth {
        deleteJsonRoute {
          walletNotExists(wallet).getOrElse {
            if (!PublicKey25519Proposition.validPubKey(address).isSuccess) {
              ApiError.invalidAddress
            } else {
              val deleted = wallet.correspondingSecret(address).exists(account => wallet.deleteSecret(account))
              Map("deleted" -> deleted).asJson
            }
          }
        }
      }
    }
  }

  @Path("/sign/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "String"),
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"Base58-encoded\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}")
  ))
  def sign: Route = {
    path("sign" / Segment) { case address =>
      signPath(address, encode = true)
    }
  }

  @Path("/signText/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "String"),
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"plain text\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}")
  ))
  def signText: Route = {
    path("signText" / Segment) { case address =>
      signPath(address, encode = false)
    }
  }

  @Path("/verify/{address}")
  @ApiOperation(value = "Verify", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path"),
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.SignedMessage",
      defaultValue = "{\n\t\"message\":\"Base58-encoded message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verify: Route = {
    path("verify" / Segment) { case address =>
      verifyPath(address, decode = true)
    }
  }

  @Path("/verifyText/{address}")
  @ApiOperation(value = "Verify text", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path"),
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.SignedMessage",
      defaultValue = "{\n\t\"message\":\"Plain message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verifyText: Route = {
    path("verifyText" / Segment) { case address =>
      verifyPath(address, decode = false)
    }
  }

  @Path("/balance/{address}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def balance: Route = {
    path("balance" / Segment) { case address =>
      getJsonRoute {
        balanceJson(address)
      }
    }
  }


  @Path("/seed/{address}")
  @ApiOperation(value = "Seed", notes = "Export seed value for the {address}", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def seed: Route = {
    path("seed" / Segment) { case address => //todo: address isn't needed
      getJsonRoute {
        //TODO CHECK IF WALLET EXISTS
        withPrivateKeyAccount(wallet, address) { account =>
          Map("address" -> address, "seed" -> Base58.encode(wallet.seedOpt.get)).asJson
        }
      }
    }
  }

  @Path("/validate/{address}")
  @ApiOperation(value = "Validate", notes = "Check whether address {address} is valid or not", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def validate: Route = {
    path("validate" / Segment) { case address =>
      getJsonRoute {
        Map("address" -> address.asJson,
          "valid" -> PublicKey25519Proposition.validPubKey(address).isSuccess.asJson).asJson
      }
    }
  }

  @Path("/")
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses", httpMethod = "GET")
  def root: Route = {
    path("addresses") {
      getJsonRoute {
        wallet.privateKeyAccounts().map(a => a.publicCommitment.address).asJson
      }
    }
  }

  @Path("/seq/{from}/{to}")
  @ApiOperation(value = "Seq", notes = "Get wallet accounts addresses", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "from", value = "Start address", required = true, dataType = "Int", paramType = "path"),
    new ApiImplicitParam(name = "to", value = "address", required = true, dataType = "Int", paramType = "path")
  ))
  def seq: Route = {
    path("seq" / IntNumber / IntNumber) { case (start, end) =>
      getJsonRoute {
        wallet.privateKeyAccounts().map(_.publicAddress).slice(start, end).asJson
      }
    }
  }

  @Path("/")
  @ApiOperation(value = "Create", notes = "Create a new account in the wallet(if it exists)", httpMethod = "POST")
  def create: Route = {
    path("addresses") {
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            Map("address" -> wallet.generateNewAccount().publicCommitment.address).asJson
          }
        }
      }
    }
  }

  private def balanceJson(address: String): Json = {
    PublicKey25519Proposition.validPubKey(address) match {
      case Success(pubkey) =>
        Map(
          "address" -> address.asJson,
          "balance" -> state.balance(pubkey).asJson
        ).asJson
      case _ => ApiError.invalidAddress
    }
  }

  private def signPath(address: String, encode: Boolean) = {
    entity(as[String]) { message =>
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            if (!PublicKey25519Proposition.validPubKey(address).isSuccess) {
              ApiError.invalidAddress
            } else {
              wallet.correspondingSecret(address) match {
                case None => SimpleTransactionalModuleErrors.walletAddressNotExists
                case Some(sh) =>
                  Try(sh.sign(message.getBytes(StandardCharsets.UTF_8))) match {
                    case Success(signature) =>
                      val msg = if (encode) Base58.encode(message.getBytes) else message
                      Map("message" -> msg,
                        "publickey" -> Base58.encode(sh.publicCommitment.id),
                        "signature" -> Base58.encode(signature.bytes)).asJson
                    case Failure(t) => json(t)
                  }
              }
            }
          }
        }
      }
    }
  }


  private def verifyPath(address: String, decode: Boolean) = {
    entity(as[String]) { jsText =>
      withAuth {
        postJsonRoute {
          io.circe.parser.decode[SignedMessage](jsText).toOption match {
            case Some(m) =>
              if (!PublicKey25519Proposition.validPubKey(address).isSuccess) {
                invalidAddress
              } else {
                //DECODE SIGNATURE
                val msg: Try[Array[Byte]] = if (decode) Base58.decode(m.message) else Success(m.message.getBytes)
                (msg, Base58.decode(m.signature), Base58.decode(m.publickey)) match {
                  case (Failure(_), _, _) => invalidAddress
                  case (_, Failure(_), _) => invalidSignature
                  case (_, _, Failure(_)) => invalidPublicKey
                  case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
                    val account = PublicKey25519Proposition(Sized.wrap(pubKeyBytes))
                    val isValid = account.address == address && account.verify(msgBytes, Sized.wrap(signatureBytes))
                    ("valid" -> isValid).asJson
                }
              }
            case _ =>
              ApiError.wrongJson
          }
        }
      }
    }
  }
}
