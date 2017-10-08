package freedsl.errorhandler

import freestyle.tagless._

@tagless trait ErrorHandler {
  def exception(t: Throwable): FS[Unit]
  def errorMessage(e: String): FS[Unit]
  def get[A](either: Either[Throwable, A]): FS[A]
  def get[A](t: util.Try[A]): FS[A] = get(t.toEither)
}

case class ErrorHandlerInterpreter() extends ErrorHandler.Handler[freedsl.dsl.Evaluated] {
  def exception(t: Throwable) = freedsl.dsl.error(t)
  def errorMessage(e: String) = freedsl.dsl.error(new RuntimeException(e))
  def get[A](either: Either[Throwable, A]) = either
}

