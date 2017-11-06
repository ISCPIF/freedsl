package freedsl.system

import freestyle.tagless._
import java.util.UUID
import squants._

@tagless trait System {
  def randomUUID(): FS[UUID]
  def sleep(duration: Time): FS[Unit]
  def currentTime(): FS[Long]
}

case class SystemInterpreter() extends System.Handler[freedsl.dsl.Evaluated] {
  def randomUUID() = freedsl.dsl.guard(UUID.randomUUID())
  def sleep(d: Time) = freedsl.dsl.guard(Thread.sleep(d.millis))
  def currentTime() = freedsl.dsl.guard(java.lang.System.currentTimeMillis())
}