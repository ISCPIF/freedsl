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

import freestyle.tagless._

@tagless trait Random {
  def nextDouble(): FS[Double]
  def nextInt(n: Int): FS[Int]
  def nextBoolean(): FS[Boolean]
  def shuffle[A](s: Vector[A]): FS[Vector[A]]
  def use[T](f: util.Random => T): FS[T]
}

import util.Try

object RandomInterpreter {
  def apply(seed: Long): RandomInterpreter = new RandomInterpreter(new util.Random(seed))
  def apply(random: util.Random): RandomInterpreter = new RandomInterpreter(random)
}

class RandomInterpreter(val random: util.Random) extends Random.Handler[freedsl.dsl.Evaluated] {
  def nextDouble() = freedsl.dsl.guard(random.nextDouble())
  def nextInt(n: Int) = freedsl.dsl.guard(random.nextInt(n))
  def nextBoolean() = freedsl.dsl.guard(random.nextBoolean())
  def shuffle[A](s: Vector[A]) = freedsl.dsl.guard(random.shuffle(s))
  def use[T](f: util.Random => T) = freedsl.dsl.guard(f(random))
}