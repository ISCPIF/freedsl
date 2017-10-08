package freedsl.io

import freestyle.tagless._

@tagless trait IO {
  def apply[A](f: () => A): FS[A]
}

case class IOInterpreter() extends IO.Handler[freedsl.dsl.Evaluated] {
  def apply[A](f: () => A) = freedsl.dsl.guard(f())
}
