package freedsl.errorhandler

import freestyle.tagless._

@tagless trait ErrorHandler {
  def exception(t: Throwable): FS[Unit]
  def errorMessage(e: String): FS[Unit]
  def get[A](either: Either[Throwable, A]): FS[A] = get(either.toTry)
  def get[A](t: util.Try[A]): FS[A]
}

case class ErrorHandlerInterpreter() extends ErrorHandler.Handler[util.Try] {
  def exception(t: Throwable) = scala.util.Failure(t)
  def errorMessage(e: String) = scala.util.Failure(new RuntimeException(e))
  def get[A](t: util.Try[A]) = t
}

