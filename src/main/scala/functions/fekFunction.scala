package functions

import cats.effect.{IO, Resource}
import cats.syntax.all._
import sttp.client3.SttpBackend

import tg.api._

object fekFunction {
  val alexId: Option[Long] = sys.env.get("ALEX_ID").flatMap(_.toLongOption)
  val alexChatId: Option[Long] = sys.env.get("ALEX_CHAT_ID").flatMap(_.toLongOption)

  def resource(implicit b: SttpBackend[IO, Any]): Resource[IO, BotFunction] = Resource.pure[IO, BotFunction] { update =>
    val hasUrl = update.message.toList.flatMap(_.entities.getOrElse(Nil)).exists(x => x.`type` == "url")
    val hasPhoto = update.message.flatMap(_.photo).nonEmpty

    val isAlex = update.message.exists(m => alexId.contains(m.from.id) && alexChatId.forall(_ == m.chat.id))
    val ambient = if (isAlex) 0.1 else 0
    val userMultiplier = if (isAlex) 0.7 else 0.3
    val probability = ((if (hasUrl) 0.9 else 0) + (if (hasPhoto) 0.3 else 0)) * userMultiplier + ambient

    if (Math.random() > (1 - probability)) {
      update.message.traverse { m =>
        sendMessageAndForget(m.chat.id, m.message_id, "фек")
      }.as(true)
    } else IO { false }
  }
}
