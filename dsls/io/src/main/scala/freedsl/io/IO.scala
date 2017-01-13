package freedsl.io

import cats._
import cats.syntax.all._
import freedsl.dsl._

object IO {

  def interpreter = new Interpreter[Id] {
    def interpret[_] = {
      case run(f) => util.Try(f()).toEither.leftMap(t => IOException(t))
      case exception(t) => Left(IOException(t))
      case exceptionOrResult(e) => e.leftMap(t => IOException(t))
      case errorMessage(m) => Left(IOException(new RuntimeException(m)))
      case errorMessageOrResult(e) => e.leftMap(m => IOException(new RuntimeException(m)))
    }
  }

  case class IOException(t: Throwable) extends Error

}

@dsl trait IO[M[_]] {
  def run[A](f: () => A): M[A]
  def apply[A](f: => A) = run(() => f)
  def exception(t: Throwable): M[Unit]
  def exceptionOrResult[A](either: Either[Throwable, A]): M[A]
  def errorMessage(e: String): M[Unit]
  def errorMessageOrResult[A](either: Either[String, A]): M[A]
}
