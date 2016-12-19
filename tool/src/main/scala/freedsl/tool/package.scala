package freedsl

import cats._
import cats.data.Kleisli
import cats.implicits._

package object tool {

  implicit class MonadDecorator[M[_]: Monad, A](m: M[A]) {
    def until(end: M[Boolean]): M[A] = until(Kleisli[M, A, Boolean](_ => end))

    def until(end: Kleisli[M, A, Boolean]): M[A] = {
      def stop(a: A): Either[Unit, A] = Right(a)
      val continue: Either[Unit, A] = Left(Unit)

      def loop = Monad[M].tailRecM[Unit, A](Unit) { i =>
        val comp =
          for {
            a <- m
            b <- end.run(a)
          } yield (b, a)

        comp.map { case (e, a) => (if(e) stop(a) else continue) }
      }

      loop
    }

    def repeat(size: Int): M[Vector[A]] = {
      type Rec = (List[A], Int)

      def stop(a: List[A]): Either[Rec, List[A]] = Right(a)
      def continue(a: List[A], size: Int): Either[Rec, List[A]] = Left((a, size))

      def loop = Monad[M].tailRecM[Rec, List[A]]((List.empty, 0)) {
        case (i, s) =>
          val comp =
            for {
              a <- m
              b = s < size
            } yield (b, a)

          comp.map { case (e, a) => (if (e) stop(a :: i) else continue(a :: i, s + 1)) }
      }

      loop.map(_.reverse.toVector)
    }

  }

  implicit class KleisliDecorator[M[_]: Monad, A](m: Kleisli[M, A, A]) {
    def until(end: Kleisli[M, A, Boolean]) = Kleisli { a: A => fold(a)(end) }

    def fold(a: A)(end: Kleisli[M, A, Boolean]): M[A] = {
      def stop(a: A): Either[A, A] = Right(a)
      def continue(a: A): Either[A, A] = Left(a)

      def loop = Monad[M].tailRecM[A, A](a) { a =>
        val comp =
          for {
            ar <- m.run(a)
            b <- end.run(ar)
          } yield (b, ar)

        comp.map { case (e, a) => (if (e) stop(a) else continue(a)) }
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
