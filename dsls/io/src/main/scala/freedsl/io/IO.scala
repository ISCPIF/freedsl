package freedsl.io

import cats._
import cats.syntax.all._
import freedsl.dsl._

object IO {

  def interpreter = new Interpreter {
    def run[A](f: () => A)(implicit context: Context) = util.Try(f()) match {
      case util.Success(s) => success(s)
      case util.Failure(t) => failure(IOError(t))
    }
    def exception(t: Throwable)(implicit context: Context) = failure(IOError(t))
    def exceptionOrResult[A](e: Either[Throwable, A])(implicit context: Context) = e match {
      case Right(v) => success(v)
      case Left(v) => failure(IOError(v))
    }
    def errorMessage(m: String)(implicit context: Context) = failure(IOError(new RuntimeException(m)))
    def errorMessageOrResult[A](e: Either[String, A])(implicit context: Context)  = e match {
      case Right(v) => success(v)
      case Left(v) => failure(IOError(new RuntimeException(v)))
    }
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
