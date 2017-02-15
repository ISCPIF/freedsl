package freedsl.errorhandle

import freedsl.dsl._

object ErrorHandler {

  def interpreter = new Interpreter {
    def exception(t: Throwable)(implicit context: Context) = failure(RuntimeError(t))

    def exceptionOrResult[A](e: Either[Throwable, A])(implicit context: Context) = e match {
      case Right(v) => success(v)
      case Left(v) => failure(RuntimeError(v))
    }

    def errorMessage(m: String)(implicit context: Context) = failure(RuntimeError(new RuntimeException(m)))

    def errorMessageOrResult[A](e: Either[String, A])(implicit context: Context) = e match {
      case Right(v) => success(v)
      case Left(v) => failure(RuntimeError(new RuntimeException(v)))
    }
  }

  case class RuntimeError(e: Throwable) extends Exception(e.getMessage, e) with freedsl.dsl.Error
}

@dsl trait ErrorHandler[M[_]] {
  def exception(t: Throwable): M[Unit]
  def exceptionOrResult[A](either: Either[Throwable, A]): M[A]
  def exceptionOrResult[A](t: util.Try[A]): M[A] = exceptionOrResult(t.toEither)
  def errorMessage(e: String): M[Unit]
  def errorMessageOrResult[A](either: Either[String, A]): M[A]
}
