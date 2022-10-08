package functions

import cats.effect.{IO, Resource, Ref}
import cats.syntax.all._
import sttp.client3.SttpBackend

import java.time.{Instant, Duration}
import scala.math.Ordering.Implicits._
import tg.api._

import scala.util.matching.Regex

object ornulFunction {
  case class OrnulState(
    count: Int,
    last: Instant,
    randv: Double
  )

  case class SecOrnulState(
    count: Int,
    last: Instant,
    randv: Double
  )

  private val ornulRegex: Regex = "^орнул|^ор$|\\sорнул".r

  def resource(ornulRate: Int, ornulDelay: Duration, ornulTooRate: Int)(using b: SttpBackend[IO, Any]): Resource[IO, BotFunction] = Resource.eval {
    for {
      primRef <- Ref.of[IO, Map[Long, OrnulState]](Map.empty)
      secRef <- Ref.of[IO, Map[Long, OrnulState]](Map.empty)
    } yield {
      val primary: BotFunction = { update =>
        update.message.traverse { m =>
          val chatId = m.chat.id

          def update(state: OrnulState): IO[Unit] =
            primRef.update(_ + (chatId -> state))

          primRef.get.map(_.get(chatId)).flatMap {
            case Some(chatState) =>
              val msgTime = Instant.ofEpochSecond(m.date)
              val delay = ornulDelay plusSeconds (chatState.randv * 600).toInt

              if (chatState.count < ornulRate) {
                update(chatState.copy(count = chatState.count + 1)).as(false)
              } else if (Duration.between(chatState.last, msgTime) > delay) {
                for {
                  _ <- sendMessageAndForget(chatId, m.message_id, "орнул")
                  _ <- update(OrnulState(0, msgTime, Math.random()))
                } yield true
              } else {
                IO.pure(false)
              }
            case None =>
              update(OrnulState(1, Instant.MIN, Math.random())).as(false)
          }
        }.map(_.getOrElse(false))
      }

      val ornuls = List(
        "тож орнул",
        "+",
        "+ кст",
        "++"
      )

      val secondary: BotFunction = { update =>
        update.message.traverse { m =>
          val chatId = m.chat.id

          def update(state: OrnulState): IO[Unit] =
            secRef.update(_ + (chatId -> state))

          if (m.text.orElse(m.caption).exists(ornulRegex.matches)) {
            secRef.get.map(_.get(chatId)).flatMap {
              case Some(chatState) =>
                val msgTime = Instant.ofEpochSecond(m.date)

                if (chatState.count < ornulTooRate + ornulTooRate * chatState.randv / 2) {
                  update(chatState.copy(count = chatState.count + 1)).as(false)
                } else {
                  for {
                    _ <- sendMessageAndForget(chatId, m.message_id, ornuls((Math.random() * ornuls.size).toInt))
                    _ <- update(OrnulState(0, msgTime, Math.random()))
                  } yield true
                }
              case None =>
                update(OrnulState(1, Instant.MIN, Math.random())).as(false)
            }
          } else IO.pure(false)
        }.map(_.getOrElse(false))
      }

      primary ++ secondary
    }
  }
}
