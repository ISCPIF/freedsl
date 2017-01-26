package freedsl.system

import cats._
import freedsl.dsl._
import java.util.UUID
import squants._

object System {

  def interpreter = new Interpreter {
    def randomUUID = Right(UUID.randomUUID())
    def sleep(d: Time) = Right(Thread.sleep(d.millis))
  }

}

@dsl trait System[M[_]] {
  def randomUUID: M[UUID]
  def sleep(duration: Time): M[Unit]
}
