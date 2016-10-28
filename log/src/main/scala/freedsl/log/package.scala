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
import cats._
import cats.free._

package object log {
  sealed trait Instruction[A]
  final case class Print(s: String) extends Instruction[Unit]

  type DSL = Instruction :|: NilDSL
  val DSL = freek.DSL.Make[DSL]

  def interpreter = new (Instruction ~> Id) {
    def apply[A](a: Instruction[A]) = a match {
      case Print(s) => println(s)
    }
  }

  def impl[DSL0 <: freek.DSL](implicit subDSL: SubDSL1[Instruction, DSL0]) = new Log[Free[subDSL.Cop, ?]] {
    def print(s: String) = Print(s).freek[DSL0]
  }
}
