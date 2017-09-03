package freedsl

import cats.free.Free

import scala.util.Try
import cats.implicits._
import cats.~>

package object dsl {
  type DSL[T] = cats.free.Free[Try, T]

  implicit class DSLDecorator[T](f: DSL[T]) {
    def eval = tryEval.get
    def tryEval = f.runTailRec
  }

  implicit def freestyleTaglessLiftFree[F[_]] = freestyle.tagless.freestyleTaglessLiftFree[F]

}
