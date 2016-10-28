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
package freedsl.log

import cats._
import cats.free.Free
import freek._

trait Imports {

  object Log {
    sealed trait DSL[A]
    final case class Print(s: String) extends DSL[Unit]

    type PRG = DSL :|: NilDSL
    val PRG = freek.DSL.Make[PRG]

    def interpreter = new (DSL ~> Id) {
      def apply[A](a: DSL[A]) = a match {
        case Print(s) => println(s)
      }
    }

    implicit def impl[DSL0 <: freek.DSL](implicit subDSL: SubDSL1[DSL, PRG]) = new Log[Free[subDSL.Cop, ?]] {
      def print(s: String) = Log.Print(s).freek[PRG](subDSL)
    }
  }

  trait Log[M[_]] {
    def print(s: String): M[Unit]
  }

}