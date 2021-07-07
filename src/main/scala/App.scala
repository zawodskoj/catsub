import cats.{MonadThrow, Monad}
import cats.effect.concurrent.Ref
import cats.effect.{IOApp, IO, ExitCode, Blocker}
import cats.syntax.all._
import models._
import sttp.client3._
import sttp.client3.httpclient.fs2.HttpClientFs2Backend
import sttp.client3.circe.asJson

import java.time.{Instant, Duration => JavaDuration}
import scala.concurrent.duration._
import scala.math.Ordering.Implicits._
import scala.util.matching.Regex

object App extends IOApp {
  val sixHours = JavaDuration.ofHours(6)

  val token = sys.env.getOrElse("BOT_TOKEN", throw new Exception("no way"))
  val base = s"https://api.telegram.org/bot$token"

  implicit class RequestOps[T, R](r: RequestT[Identity, Either[ResponseException[String, io.circe.Error], T], R]) {
    def sendAndUnwrap[F[_]: MonadThrow](b: SttpBackend[F, R]) =
      r.send(b).flatMap(_.body.liftTo[F])
  }

  def getUpdates(offset: Long) = basicRequest
    .get(uri"$base/getUpdates?offset=$offset")
    .response(asJson[GetUpdatesResponse])
    .mapResponseRight(_.result)

  def sendMessage(chatId: Long, replyTo: Long, text: String) = basicRequest
    .post(uri"$base/sendMessage?chat_id=$chatId&text=$text&reply_to_message_id=$replyTo&parse_mode=MarkdownV2")
    .response(asJson[SendMessageResponse])
    .mapResponseRight(_.result)

  def editMessage(chatId: Long, messageId: Long, text: String) = basicRequest
    .post(uri"$base/editMessageText?chat_id=$chatId&text=$text&message_id=$messageId&parse_mode=MarkdownV2")

  case class Subst(origSedId: Long, substMessageId: Long)

  case class SubstCache(
    lastUpdated: Instant,
    substs: Vector[Subst]
  )

  case class Context(
    b: SttpBackend[IO, Any],
    offsetRef: Ref[IO, Long],
    substsRef: Ref[IO, Map[Long, SubstCache]]
  )

  case class SedFlags(
    g: Boolean,
    i: Boolean
  )

  case class SedCommand(rgx: Regex, subst: String, flags: SedFlags)

  def tryDecodeSedCommand(c: String): Option[Vector[SedCommand]] =
    c.linesIterator.map(_.replace("\\/", "\u0000")).toVector.traverse {
      case s"s/$rgx/$substWSpaces/$flagsStr" =>
        try {
          val subst = substWSpaces.stripTrailing

          val flags = SedFlags(
            g = flagsStr.contains("g"),
            i = flagsStr.contains("i")
          )

          val r = {
            val rstr = rgx.replace("\u0000", "\\/")
            var flagStr = ""

            if (flags.i)
              flagStr += "i"

            if (flagStr.isEmpty)
              rstr
            else
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

          Some(SedCommand(r, subst, SedFlags(g = false, i = false)))
        } catch {
          case _: Throwable => None
        }
      case _ => None
    }

  def handle(update: Update)(implicit context: Context): IO[Unit] = {
    import context._

    update.message
      .orElse(update.edited_message)
      .flatMap(x => x.reply_to_message.map(x -> _)) match {
      case Some((message, replyTo)) =>
        (message.text.flatMap(tryDecodeSedCommand), replyTo.text) match {
          case (Some(msgSeds), Some(replyToText)) =>
            try {
              val result = msgSeds.foldLeft(replyToText) { case (msg, sed) =>
                if (sed.flags.g) {
                  sed.rgx.replaceSomeIn(msg, v => if (v.start == v.end) None else Some(sed.subst))
                } else
                  sed.rgx.replaceFirstIn(msg, sed.subst)
              }

              for {
                substs <- context.substsRef.get
                _ <- substs.get(message.chat.id).flatMap(_.substs.find(_.origSedId == message.message_id)) match {
                  case Some(sed) =>
                    editMessage(message.chat.id, sed.substMessageId, result).send(b)
                  case _ =>
                    sendMessage(message.chat.id, replyTo.message_id, result).sendAndUnwrap(b).flatMap { m =>
                      context.substsRef.update(_.updatedWith(message.chat.id) { v =>
                        val x = v.getOrElse(SubstCache(Instant.MIN, Vector.empty))
                        val newSubsts = x.substs.takeRight(4) :+ Subst(message.message_id, m.message_id)
                        Some(x.copy(lastUpdated = Instant.now, substs = newSubsts))
                      })
                    }
                }
              } yield ()
            } catch {
              case _: Throwable => IO.unit
            }
          case _ => IO.unit
        }

      case _ => IO.unit
    }
  }

  def gc(implicit context: Context): IO[Unit] = {
    val now = Instant.now

    context.substsRef.update(_.filter { case (_, v) => JavaDuration.between(v.lastUpdated, now) < sixHours })
  }

  def loop(implicit context: Context): IO[Unit] = {
    import context._

    for {
      offset <- offsetRef.get
      updates <- getUpdates(offset).sendAndUnwrap(b)
      _ <- updates.traverse(handle)
      _ <- updates.maxByOption(_.update_id).map(_.update_id + 1).traverse(offsetRef.set)
      _ <- gc
      _ <- IO.sleep(200.millis)
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
    for {
      backend <- HttpClientFs2Backend[IO](blocker)
      offsetRef <- Ref.of[IO, Long](0)
      substsRef <- Ref.of[IO, Map[Long, SubstCache]](Map.empty)
      implicit0(context: Context) = Context(backend, offsetRef, substsRef)
      _ <- loop.foreverM
    } yield ExitCode.Success
  }
}
