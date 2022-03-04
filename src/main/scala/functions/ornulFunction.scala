package functions

import cats.effect.concurrent.Ref
import cats.effect.{IO, Resource}
import cats.syntax.all._
import sttp.client3.SttpBackend

import java.time.{Instant, Duration}
import scala.math.Ordering.Implicits._
import tg.api._

object ornulFunction {
  case class OrnulState(
    count: Int,
    last: Instant,
    randv: Double
  )

  def resource(ornulRate: Int, ornulDelay: Duration)(implicit b: SttpBackend[IO, Any]): Resource[IO, BotFunction] = Resource.eval {
    for {
      stateRef <- Ref.of[IO, Map[Long, OrnulState]](Map.empty)
    } yield { update =>
      update.message.traverse { m =>
        val chatId = m.chat.id

        def update(state: OrnulState): IO[Unit] =
          stateRef.update(_ + (chatId -> state))

        stateRef.get.map(_.get(chatId)).flatMap {
          case Some(chatState) =>
            val msgTime = Instant.ofEpochSecond(m.date)
            val delay = ornulDelay plusSeconds (chatState.randv * 600).toInt

            if (chatState.count < ornulRate) {
              update(chatState.copy(count = chatState.count + 1)).as(false)
            } else if (Duration.between(chatState.last, msgTime) > delay) {
              for {
                _ <- sendMessage(chatId, m.message_id, "орнул")
                _ <- update(OrnulState(0, msgTime, Math.random()))
              } yield false
            } else {
              IO.pure(false)
            }
          case None =>
            update(OrnulState(1, Instant.MIN, Math.random())).as(false)
        }
      }.map(_.getOrElse(false))
    }: BotFunction
  }
}
