/**
  * Created by Romain Reuillon on 27/10/16.
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Affero General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
package freedsl

import cats._
import cats.free._
import freek._

package object random {

  trait RNG[M[_]] {
    def nextDouble: M[Double]
    def nextInt(n: Int): M[Int]
  }

  object RNG {
    sealed trait DSL[A]
    final case object NextDouble extends DSL[Double]
    final case class NextInt(n: Int) extends DSL[Int]

    type PRG = DSL :|: NilDSL
    val PRG = DSL.Make[PRG]

    def interpreter(random: util.Random) = new (DSL ~> Id) {
      def apply[A](a: DSL[A]) = a match {
        case NextDouble => random.nextDouble
        case NextInt(n) => random.nextInt(n)
      }
    }

    def interpreter(seed: Long): DSL ~> Id = interpreter(new util.Random(seed))

    implicit def impl[DSL0 <: freek.DSL](implicit subDSL: SubDSL1[RNG.DSL, DSL0]) = new RNG[Free[subDSL.Cop, ?]] {
      def nextDouble = RNG.NextDouble.freek[DSL0]
      def nextInt(n: Int) = RNG.NextInt(n).freek[DSL0]
    }
  }




}
