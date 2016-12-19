package freedsl

import cats._
import cats.implicits._

package object random {

  def multinomial[M[_]: Monad, T](workingStats: List[(T, Double)])(implicit randomM: Random[M]) = {
    lazy val all = workingStats
    def roulette(weights: List[(T, Double)], selected: Double): M[T] =
      weights match {
        case Nil => randomElement[M, (T, Double)](all.toVector).map(_._1)
        case (i, p) :: t =>
          if (selected <= p) i.pure[M]
          else roulette(t, selected - p)
      }
    randomM.nextDouble.map(d => roulette(workingStats, d))
  }

  def randomElement[M[_]: Monad, T](v: Vector[T])(implicit randomM: Random[M]) =
    randomM.nextInt(v.size).map(v.apply)

}
