package freedsl.util

import cats._
import freedsl.dsl._
import java.util.UUID

object Util {

  def interpreter = new Interpreter[Id] {
    def interpret[_] = {
      case randomUUID() => Right(UUID.randomUUID())
    }
  }

}

@dsl trait Util[M[_]] {
  def randomUUID: M[UUID]
}
