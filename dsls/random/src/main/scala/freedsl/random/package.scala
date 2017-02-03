package freedsl

import cats._
import cats.implicits._

package object random {

  def multinomial[M[_]: Monad, T](elements: Vector[(T, Double)])(implicit randomM: Random[M], context: dsl.Context) = {
    def roulette(weights: List[(T, Double)], selected: Double): M[T] =
      weights match {
        case Nil => randomElement[M, (T, Double)](elements).map(_._1)
        case (i, p) :: t =>
          if (selected <= p) i.pure[M]
          else roulette(t, selected - p)
      }
    randomM.nextDouble.flatMap(d => roulette(elements.toList, d))
  }

  def randomElement[M[_]: Functor, T](v: Vector[T])(implicit randomM: Random[M], context: dsl.Context) =
    randomM.nextInt(v.size).map(v.apply)

  implicit class RandomDecorator[M[_]](randomM: Random[M]) {
    private implicit def implicitRandomM = randomM
    def multinomial[T](v: Vector[(T, Double)])(implicit monad: Monad[M], context: dsl.Context) = random.multinomial[M, T](v)
    def randomElement[T](v: Vector[T])(implicit functor: Functor[M], context: dsl.Context) = random.randomElement[M, T](v)
  }

}
