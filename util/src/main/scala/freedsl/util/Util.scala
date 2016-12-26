package freedsl.util

import cats._
import freedsl.dsl._
import java.util.UUID
import squants._

object Util {

  def interpreter = new Interpreter[Id] {
    def interpret[_] = {
      case randomUUID() => Right(UUID.randomUUID())
      case sleep(d) => Right(Thread.sleep(d.millis))
    }
  }

}

@dsl trait Util[M[_]] {
  def randomUUID: M[UUID]
  def sleep(duration: Time): M[Unit]
}
