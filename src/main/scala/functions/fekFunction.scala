package functions

import cats.effect.{IO, Resource}
import cats.syntax.all._
import sttp.client3.SttpBackend

import tg.api._

object fekFunction {
  val alexId: Option[Long] = sys.env.get("ALEX_ID").flatMap(_.toLongOption)
  val alexChatId: Option[Long] = sys.env.get("ALEX_CHAT_ID").flatMap(_.toLongOption)
  val isEnabled: Boolean = sys.env.get("FEK_ENABLED").flatMap(_.toBooleanOption).getOrElse(true)

  def resource(implicit b: SttpBackend[IO, Any]): Resource[IO, BotFunction] = Resource.pure[IO, BotFunction] { update =>
    val hasUrl = update.message.toList.flatMap(_.entities.getOrElse(Nil)).exists(x => x.`type` == "url")
    val hasPhoto = update.message.flatMap(_.photo).nonEmpty
    val hasNews = update.message.flatMap(_.text).getOrElse("").contains("⚡️")

    val isAlex = update.message.exists(m => alexId.contains(m.from.id) && alexChatId.forall(_ == m.chat.id))
    val ambient = if (isAlex) 0.1 else 0
    val userMultiplier = if (isAlex) 0.7 else 0.2

    val rawProbability = (hasNews, hasUrl, hasPhoto) match {
      case (true, _, _) => 1
      case (_, true, true) => 0.8
      case (_, true, _) => 0.6
      case (_, _, true) => 0.2
      case _ => 0.05
    }

    val probability = rawProbability * userMultiplier + ambient

    if (isEnabled && (Math.random() > (1 - probability))) {
      update.message.traverse { m =>
        sendMessageAndForget(m.chat.id, m.message_id, "фек")
      }.as(true)
    } else IO { false }
  }
}
