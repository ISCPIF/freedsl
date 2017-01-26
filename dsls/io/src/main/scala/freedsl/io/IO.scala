package freedsl.io

import cats._
import cats.syntax.all._
import freedsl.dsl._

object IO {

  def interpreter = new Interpreter {
    def run[A](f: () => A) = util.Try(f()) match {
      case util.Success(s) => Right(s)
      case util.Failure(t) => Left(IOError(t))
    }
    def exception(t: Throwable) = Left(IOError(t))
    def exceptionOrResult[A](e: Either[Throwable, A]) = e.leftMap(t => IOError(t))
    def errorMessage(m: String) = Left(IOError(new RuntimeException(m)))
    def errorMessageOrResult[A](e: Either[String, A]) = e.leftMap(m => IOError(new RuntimeException(m)))
  }

  case class IOError(t: Throwable) extends Error

}

@dsl trait IO[M[_]] {
  def run[A](f: () => A): M[A]
  def apply[A](f: => A) = run(() => f)
  def exception(t: Throwable): M[Unit]
  def exceptionOrResult[A](either: Either[Throwable, A]): M[A]
  def errorMessage(e: String): M[Unit]
  def errorMessageOrResult[A](either: Either[String, A]): M[A]
}
