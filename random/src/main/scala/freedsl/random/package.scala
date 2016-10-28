/**
  * Created by Romain Reuillon on 28/10/16.
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

import freek._

package object random {

  import cats._
  import free._

  sealed trait Instruction[A]
  final case object NextDouble extends Instruction[Double]
  final case class NextInt(n: Int) extends Instruction[Int]

  type DSL = Instruction :|: NilDSL
  val DSL = freek.DSL.Make[DSL]

  def interpreter(random: util.Random) = new (Instruction ~> Id) {
    def apply[A](a: Instruction[A]) = a match {
      case NextDouble => random.nextDouble
      case NextInt(n) => random.nextInt(n)
    }
  }

  def interpreter(seed: Long): Instruction ~> Id = interpreter(new util.Random(seed))

  def impl[DSL0 <: freek.DSL](implicit subDSL: freek.SubDSL1[Instruction, DSL0]) = new Random[Free[subDSL.Cop, ?]] {
    def nextDouble = NextDouble.freek[DSL0]
    def nextInt(n: Int) = NextInt(n).freek[DSL0]
  }
}
