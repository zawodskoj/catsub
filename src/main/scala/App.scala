import cats.effect.concurrent.Ref
import cats.effect.{Blocker, Resource, ExitCode, IOApp, IO}
import cats.syntax.all._
import sttp.client3._
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend

import java.time.{Instant, Duration => JavaDuration}
import scala.concurrent.duration._
import tg.api._
import functions._

object App extends IOApp {
  val ornulRate = 100
  val ornulTooRate = 20
  val ornulDelay: JavaDuration = JavaDuration.ofMinutes(30)
  
  val ignoreRewindFlag: Boolean = sys.env.getOrElse("BOT_NO_REWIND", "true").toLowerCase match {
    case "yes" | "true" | "y" | "t" | "1" => true
    case _ => false
  }
  
  val deployDate: Long = Instant.now.getEpochSecond

  def mkFn(implicit b: SttpBackend[IO, Any]): Resource[IO, BotFunction] =
    (antiKpopFunction.resource, sedFunction.resource, tyanochkuFunction.resource, ornulFunction.resource(ornulRate, ornulDelay, ornulTooRate))
      .mapN { (antiKpop, sed, tyan, ornul) => antiKpop ++ sed ++ tyan ++ ornul }

  def isUpdateFreshEnough(x: models.Update): Boolean =
    x.message.orElse(x.edited_message).exists(_.date > deployDate)
  
  def loop(offsetRef: Ref[IO, Long], fn: BotFunction)(implicit b: SttpBackend[IO, Any]): IO[Unit] =
    for {
      offset <- offsetRef.get
      updates <- getUpdates(offset)
      validUpdates = if (ignoreRewindFlag) updates.filter(isUpdateFreshEnough) else updates
      _ <- validUpdates.traverse_(fn.handleUpdate)
      _ <- validUpdates.traverse_(u => if (isUpdateFreshEnough(u)) fn.handleUpdate(u) else IO.unit)
      _ <- updates.maxByOption(_.update_id).map(_.update_id + 1).traverse(offsetRef.set)
      _ <- IO.sleep(200.millis)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    val res = for {
      blocker <- Blocker[IO]
      implicit0(b: SttpBackend[IO, Any]) <- AsyncHttpClientFs2Backend.resource[IO](blocker)
      fn <- mkFn
      _ <- Resource.eval {
        Ref.of[IO, Long](0).flatMap(offsetRef => loop(offsetRef, fn).foreverM)
      }
    } yield ExitCode.Success

    res.use(IO.pure)
  }
}
