package tg

import cats.effect.IO
import cats.syntax.all._
import models._
import sttp.client3._
import sttp.client3.circe._

object api {
  val token: String = sys.env.getOrElse("BOT_TOKEN", throw new Exception("no way"))
  val base = s"https://api.telegram.org/bot$token"

  trait AsException[-E] {
    def exception(value: E): Exception
  }
  object AsException {
    implicit val exceptionAsException: AsException[Exception] = x => x
    implicit val stringAsException: AsException[String] = x => new Exception(x)
  }

  implicit class RequestOps[T, R, E](r: RequestT[Identity, Either[E, T], R]) {
    def sendAndUnwrap(b: SttpBackend[IO, R])(implicit asException: AsException[E]): IO[T] =
      r.send(b).flatMap(_.body.leftMap(asException.exception).liftTo[IO])
  }

  def getUpdates(offset: Long)(implicit b: SttpBackend[IO, Any]): IO[Vector[Update]] = basicRequest
    .get(uri"$base/getUpdates?offset=$offset")
    .response(asJson[GetUpdatesResponse])
    .mapResponseRight(_.result)
    .sendAndUnwrap(b)

  def sendMessage(chatId: Long, replyTo: Long, text: String)(implicit b: SttpBackend[IO, Any]): IO[Message] = basicRequest
    .post(uri"$base/sendMessage?chat_id=$chatId&text=$text&reply_to_message_id=$replyTo")
    .response(asJson[SendMessageResponse])
    .mapResponseRight(_.result)
    .sendAndUnwrap(b)

  def editMessage(chatId: Long, messageId: Long, text: String)(implicit b: SttpBackend[IO, Any]): IO[Unit] = basicRequest
    .post(uri"$base/editMessageText?chat_id=$chatId&text=$text&message_id=$messageId")
    .sendAndUnwrap(b)
    .void
}
