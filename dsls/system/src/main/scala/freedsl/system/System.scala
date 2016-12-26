package freedsl.system

import cats._
import freedsl.dsl._
import java.util.UUID
import squants._

object System {

  def interpreter = new Interpreter[Id] {
    def interpret[_] = {
      case randomUUID() => Right(UUID.randomUUID())
      case sleep(d) => Right(Thread.sleep(d.millis))
    }
  }

}

@dsl trait System[M[_]] {
  def randomUUID: M[UUID]
  def sleep(duration: Time): M[Unit]
}
