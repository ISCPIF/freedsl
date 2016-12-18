package freedsl.io

import cats.Id
import freedsl.dsl._


object IO {

  def interpreter = new Interpreter[Id] {
    def interpret[_] = {
      case run(f) =>
        util.Try(f()) match {
          case util.Success(r) => Right(r)
          case util.Failure(t) => Left(IOError(t))
        }
    }
  }

  case class IOError(t: Throwable) extends Error

}

@dsl trait IO[M[_]] {
  def run[A](f: () => A): M[A]
  def apply[A](f: => A) = run(() => f)
}
