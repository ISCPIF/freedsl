package freedsl

import scala.util.Try
import cats.implicits._

package object dsl {
  type DSL[T] = cats.free.Free[Try, T]

  implicit class DSLDecorator[T](f: DSL[T]) {
    def eval = tryEval.get
    def tryEval = f.runTailRec
  }
}
