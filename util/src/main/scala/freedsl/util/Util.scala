package freedsl.util

import cats._
import freedsl.dsl._
import java.util.UUID
import scala.concurrent.duration._

object Util {

  def interpreter = new Interpreter[Id] {
    def interpret[_] = {
      case randomUUID() => Right(UUID.randomUUID())
      case sleep(d) => Right(Thread.sleep(d.toMillis))
    }
  }

}

@dsl trait Util[M[_]] {
  def randomUUID: M[UUID]
  def sleep(duration: Duration): M[Unit]
}
