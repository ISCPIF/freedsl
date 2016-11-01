/**
  * Created by Romain Reuillon on 01/11/16.
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
package freedsl.dsl

import cats._
import cats.data._
import freek._
import cats.implicits._


object SimpleDSL {
  object DSLTestM {
    def interpreter = new Interpreter[Id] {
      def interpret[_] = {
        case get() => 0
        case getH() => Set(0)
      }
    }
  }

  @dsl trait DSLTestM[M[_]] {
    def get: M[Int]
    def getH: M[Set[Int]]
  }

  type DSL = DSLTestM.DSL
  val DSL = freek.DSL.Make[DSL]

  def impl =  DSLTestM.impl[DSL]


  type O = Option :&: Bulb
  DSLTestM.get().freek[DSL].onionT[O]
  //DSLTestM.getH().freek[DSL].onionT[O] // Not working yet
}

object DSLWithOnionTest {

//  object DSLTestM {
//
//    def interpreter = new Interpreter[Id] {
//      var state: Option[Int] = None
//
//      def interpret[_] = {
//        case get() => state match {
//          case Some(v) => Xor.right(v): Xor[String, Int]
//          case None => Xor.left("V has not been set"): Xor[String, Int]
//        }
//      }
//    }
//
//  }
//
//  @dsl trait DSLTestM[M[_]] {
//    def get: M[Xor[String, Int]]
//  }
//
//  type DSL = DSLTestM.DSL
//  val DSL = freek.DSL.Make[DSL]
//
//  //type O = Xor[String, ?] :&: Bulb
//
//  def impl = new DSLTestM[cats.free.Free[DSL.Cop, ?]] {
//    def get = ToFreek1(DSLTestM.get()).freek[DSL]
//  }


}
