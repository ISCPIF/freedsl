package freedsl.io

import freedsl.dsl._

object IO {

  def interpreter = new Interpreter {
    def run[A](f: () => A)(implicit context: Context) = util.Try(f()) match {
      case util.Success(s) => success(s)
      case util.Failure(t) => failure(IOError(t))
    }
  }

  case class IOError(t: Throwable) extends Error

}

@dsl trait IO[M[_]] {
  def run[A](f: () => A): M[A]
  def apply[A](f: => A) = run(() => f)
}
