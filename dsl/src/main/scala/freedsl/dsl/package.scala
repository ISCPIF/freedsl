package freedsl

import cats.free.Free
import cats.implicits._
import cats.~>

package object dsl {

  def error(f: => Throwable) = Left(f)
  def result[T](f: => T) = Right(f)
  def guard[T](f: => T) =
    try result(f)
    catch {
      case t: Throwable => error(t)
    }

  type Evaluated[T] = Either[Throwable, T]
  type DSL[T] = cats.free.Free[Evaluated, T]

  implicit class DSLDecorator[T](f: DSL[T]) {
    def eval = tryEval.get
    def tryEval = f.runTailRec.toTry
  }

  implicit def freestyleTaglessLiftFree[F[_]] = freestyle.tagless.freestyleTaglessLiftFree[F]

}
