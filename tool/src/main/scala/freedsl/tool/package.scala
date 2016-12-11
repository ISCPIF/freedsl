package freedsl

import cats._
import cats.implicits._

package object tool {

  implicit class MonadDecorator[M[_]: Monad, A](m: M[A]) {
    def until(end: M[Boolean]): M[A] = {
      def stop(a: A): Either[Unit, A] = Right(a)
      val continue: Either[Unit, A] = Left(Unit)

      def loop = Monad[M].tailRecM[Unit, A](Unit) { i =>
        val comp =
          for {
            a <- m
            b <- end
          } yield (b, a)

        comp.map { case (e, a) => (if(e) stop(a) else continue) }
      }

      loop
    }
  }

  def modifier[F[_] : Monad, T](get: F[T], set: T => F[Unit]) = new {
    def modify(f: T => T) =
      for {
        v <- get
        nv = f(v)
        _ <- set(nv)
      } yield nv

    def apply(f: T => T) = modify(f)
  }

}
