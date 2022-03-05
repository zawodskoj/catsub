package functions

import cats.effect.concurrent.Ref
import cats.effect.{IO, Resource}
import cats.syntax.all._
import sttp.client3.SttpBackend

import java.time.Instant
import scala.util.matching.Regex
import tg.api._

object sedFunction {
  case class Subst(origSedId: Long, substMessageId: Long)

  case class SubstCache(
    lastUpdated: Instant,
    substs: Vector[Subst]
  )

  case class SedFlags(
    g: Boolean,
    i: Boolean,
    d: Boolean
  )

  case class SedCommand(rgx: Regex, subst: String, flags: SedFlags)

  def tryDecodeSedCommand(c: String): Option[Vector[SedCommand]] =
    c.linesIterator.map(_.replace("\\/", "\u0000")).toVector.traverse {
      case s"s/$rgx/$substWSpaces/$flagsStr" =>
        try {
          val subst = substWSpaces.stripTrailing

          val flags = SedFlags(
            g = flagsStr.contains("g"),
            i = flagsStr.contains("i"),
            d = flagsStr.contains("d")
          )

          val r = {
            val rstr = rgx.replace("\u0000", "\\/")
            var flagStr = "u"

            if (flags.i)
              flagStr += "i"

            s"(?$flagStr)$rstr"
          }.r

          Some(SedCommand(r, subst, flags))
        } catch {
          case _: Throwable => None
        }
      case s"s/$rgx/$substWSpaces" =>
        try {
          val subst = substWSpaces.stripTrailing
          val r = rgx.replace("\u0000", "\\/").r

          Some(SedCommand(r, subst, SedFlags(g = false, i = false, d = false)))
        } catch {
          case _: Throwable => None
        }
      case _ => None
    }

  def applySed(msg: String, sed: SedCommand): String =
    if (sed.flags.g) {
      sed.rgx.replaceSomeIn(msg, v => if (v.start == v.end) None else Some(sed.subst))
    } else {
      sed.rgx.replaceFirstIn(msg, sed.subst)
    }

  def resource(implicit b: SttpBackend[IO, Any]): Resource[IO, BotFunction] = Resource.eval {
    for {
      cacheRef <- Ref.of[IO, Map[Long, SubstCache]](Map.empty)
    } yield { update =>
      val maybeAction = for {
        commandMessage <- update.message.orElse(update.edited_message)
        chatId = commandMessage.chat.id
        replyToMessage <- commandMessage.reply_to_message
        (commandText, replyToText) <- (commandMessage.textOrCaption, replyToMessage.textOrCaption).tupled
        decodedSed <- tryDecodeSedCommand(commandText)
        resultText = decodedSed.foldLeft(replyToText)(applySed)
      } yield cacheRef.get
        .map(_.get(chatId).flatMap(_.substs.find(_.origSedId == commandMessage.message_id)))
        .flatMap {
          case Some(sed) => editMessage(chatId, sed.substMessageId, resultText)
          case None =>
            if (decodedSed.exists(_.flags.d))
              for {
                _ <- sendMessage(chatId, replyToMessage.message_id, resultText)
                _ <- deleteMessage(chatId, commandMessage.message_id)
              } yield ()
            else
              for {
                message <- sendMessage(chatId, replyToMessage.message_id, resultText)
                _ <- cacheRef.update(_.updatedWith(chatId) { v =>
                  val entry = v.getOrElse(SubstCache(Instant.MIN, Vector.empty))
                  val newSubsts = entry.substs.takeRight(4) :+ Subst(commandMessage.message_id, message.message_id)

                  Some(entry.copy(lastUpdated = Instant.now, substs = newSubsts))
                })
              } yield ()
        }
        .as(true)

      maybeAction.getOrElse(IO.pure(false))
    }: BotFunction
  }
}
