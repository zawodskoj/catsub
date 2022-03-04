import cats.effect.IO
import models._

package object functions {
  trait BotFunction {
    def handleUpdate(update: Update): IO[Boolean]

    def ++(f: BotFunction): BotFunction = u => handleUpdate(u).flatMap { x => if (!x) f.handleUpdate(u) else IO.pure(x) }
  }
}
