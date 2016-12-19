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
package freedsl.random

import cats._
import cats.implicits._
import freedsl.dsl._

object Random {

  def interpreter(random: util.Random) = new Interpreter[Id] {
    def interpret[_] = {
      case nextDouble() => Right(random.nextDouble)
      case nextInt(n) => Right(random.nextInt(n))
      case nextBoolean() => Right(random.nextBoolean())
      case shuffle(s) => Right(random.shuffle(s))
      case use(f) => Right(f(random))
    }
  }

  def interpreter(seed: Long): Interpreter[Id] = interpreter(new util.Random(seed))

}

@dsl trait Random[M[_]] {
  def nextDouble: M[Double]
  def nextInt(n: Int): M[Int]
  def nextBoolean: M[Boolean]
  def shuffle[A](s: Vector[A]): M[Vector[A]]
  def use[T](f: util.Random => T): M[T]
}
