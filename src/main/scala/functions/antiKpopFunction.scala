package functions

import cats.effect.{IO, Resource}
import cats.syntax.all._
import sttp.client3.SttpBackend
import tg.api.deleteMessage

object antiKpopFunction {
  def resource(using b: SttpBackend[IO, Any]): Resource[IO, BotFunction] = Resource.pure[IO, BotFunction] {
    { update =>
      update.message.flatMap(_.sticker).flatMap(_.set_name).traverse { m =>
        if (m === "runrunrun_by_stickersthiefbot") {
          deleteMessage(update.message.get.chat.id, update.message.get.message_id).as(false)
        } else {
          IO.pure(false)
        }
      }.map(_.getOrElse(false))
    } : BotFunction
  }
}
