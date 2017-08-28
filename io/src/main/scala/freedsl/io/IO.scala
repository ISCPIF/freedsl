package freedsl.io

import freestyle.tagless._

@tagless trait IO {
  def apply[A](f: () => A): FS[A]
}

case class IOInterpreter() extends IO.Handler[util.Try] {
  def apply[A](f: () => A) = util.Try(f())
}
