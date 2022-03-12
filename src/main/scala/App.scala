import cats.effect.concurrent.Ref
import cats.effect.{Blocker, Resource, ExitCode, IOApp, IO}
import cats.syntax.all._
import sttp.client3._
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend

import java.time.{Duration => JavaDuration}
import scala.concurrent.duration._
import tg.api._
import functions._

object App extends IOApp {
  val ornulRate = 100
  val ornulTooRate = 20
  val ornulDelay: JavaDuration = JavaDuration.ofMinutes(30)

  def mkFn(implicit b: SttpBackend[IO, Any]): Resource[IO, BotFunction] =
    (sedFunction.resource, tyanochkuFunction.resource, ornulFunction.resource(ornulRate, ornulDelay, ornulTooRate))
      .mapN { (sed, tyan, ornul) => sed ++ tyan ++ ornul }

  def loop(offsetRef: Ref[IO, Long], fn: BotFunction)(implicit b: SttpBackend[IO, Any]): IO[Unit] =
    for {
      offset <- offsetRef.get
      updates <- getUpdates(offset)
      _ <- updates.traverse_(fn.handleUpdate)
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
