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

  def interpreter(random: util.Random) = new Interpreter {
    def nextDouble(implicit context: Context) = random.nextDouble
    def nextInt(n: Int)(implicit context: Context) = random.nextInt(n)
    def nextBoolean(implicit context: Context) = random.nextBoolean()
    def shuffle[A](s: Vector[A])(implicit context: Context) = result(random.shuffle(s))
    def use[T](f: util.Random => T)(implicit context: Context) = f(random)
  }

  def interpreter(seed: Long): Interpreter = interpreter(new util.Random(seed))

}

@dsl trait Random[M[_]] {
  def nextDouble: M[Double]
  def nextInt(n: Int): M[Int]
  def nextBoolean: M[Boolean]
  def shuffle[A](s: Vector[A]): M[Vector[A]]
  def use[T](f: util.Random => T): M[T]
}
