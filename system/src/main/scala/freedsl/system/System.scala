package freedsl.system

import freestyle.tagless._
import java.util.UUID
import squants._

@tagless trait System {
  def randomUUID(): FS[UUID]
  def sleep(duration: Time): FS[Unit]
}

case class SystemInterpreter() extends System.Handler[util.Try] {
  def randomUUID() = util.Try(UUID.randomUUID())
  def sleep(d: Time) = util.Try(Thread.sleep(d.millis))
}