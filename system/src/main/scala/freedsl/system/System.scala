package freedsl.system

import freestyle.tagless._
import java.util.UUID
import squants._

@tagless trait System {
  def randomUUID(): FS[UUID]
  def sleep(duration: Time): FS[Unit]
}

case class SystemInterpreter() extends System.Handler[freedsl.dsl.Evaluated] {
  def randomUUID() = freedsl.dsl.guard(UUID.randomUUID())
  def sleep(d: Time) = freedsl.dsl.guard(Thread.sleep(d.millis))
}