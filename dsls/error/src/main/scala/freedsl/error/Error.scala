package freedsl.error

import freedsl.dsl._

object Error {

  def interpreter = new Interpreter {
    def exception(t: Throwable)(implicit context: Context) = failure(Error(t))

    def exceptionOrResult[A](e: Either[Throwable, A])(implicit context: Context) = e match {
      case Right(v) => success(v)
      case Left(v) => failure(Error(v))
    }

    def errorMessage(m: String)(implicit context: Context) = failure(Error(new RuntimeException(m)))

    def errorMessageOrResult[A](e: Either[String, A])(implicit context: Context) = e match {
      case Right(v) => success(v)
      case Left(v) => failure(Error(new RuntimeException(v)))
    }
  }

  case class Error(e: Throwable) extends Exception(e.getMessage, e) with freedsl.dsl.Error
}

@dsl trait Error[M[_]] {
  def exception(t: Throwable): M[Unit]
  def exceptionOrResult[A](either: Either[Throwable, A]): M[A]
  def errorMessage(e: String): M[Unit]
  def errorMessageOrResult[A](either: Either[String, A]): M[A]
}
