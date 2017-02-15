package freedsl.errorhandler

import freedsl.dsl._

object ErrorHandler {

  def interpreter = new Interpreter {
    def exception(t: Throwable)(implicit context: Context) = failure(RuntimeError(t))

    def get[A](e: Either[Throwable, A])(implicit context: Context) = e match {
      case Right(v) => success(v)
      case Left(v) => failure(RuntimeError(v))
    }

    def errorMessage(m: String)(implicit context: Context) = failure(RuntimeError(new RuntimeException(m)))
  }

  case class RuntimeError(e: Throwable) extends Exception(e.getMessage, e) with freedsl.dsl.Error

  def toEither[T](t: util.Try[T]): Either[Throwable, T] =
    t match {
      case util.Success(t) => Right(t)
      case util.Failure(t) => Left(t)
    }
}

@dsl trait ErrorHandler[M[_]] {
  def exception(t: Throwable): M[Unit]
  def errorMessage(e: String): M[Unit]
  def get[A](either: Either[Throwable, A]): M[A]
  def get[A](t: util.Try[A]): M[A] = get(ErrorHandler.toEither(t))
}
