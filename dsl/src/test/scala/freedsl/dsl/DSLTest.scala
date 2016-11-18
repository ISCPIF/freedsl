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
import cats.free.Free
import freek._
import cats.implicits._

object PureFreek extends App {

  object PureFreek {
    sealed trait Instruction[T]
    final case class Get() extends Instruction[Int]
    final case class GetSet() extends Instruction[Set[Int]]
    final case class GetMap() extends Instruction[Map[Int, Int]]

    type DSL = Instruction :|: NilDSL
    type O = Option :&: Bulb

    Get().freek[DSL].onionT[O]
    GetSet().freek[DSL].onion[O]
    GetMap().freek[DSL].onion[O]

    def interpreter = new (Instruction ~> Option) {
      def apply[A](a: Instruction[A]) = a match {
        case Get() => Some(1)
        case GetSet() => Some(Set(1))
        case GetMap() => Some(Map(1 -> 1))
      }
    }

  }

  object DSLTest {

    object DSLTestM {
      def interpreter = new Interpreter[Id] {
        def interpret[_] = {
          case get() => 1
          case getSet() => Set(1)
        }
      }

      def interpreterOption = new Interpreter[Option] {
        def interpret[_] = {
          case get() => Some(1)
          case getSet() => Some(Set(1))
        }
      }
    }

    @dsl trait DSLTestM[M[_]] {
      def get: M[Int]
      def getSet: M[Set[Int]]
    }

  }

  import DSLTest._

  type DSL = DSLTestM.DSL
  val DSL = freek.DSL.Make[DSL]
  type Context[T] = Free[DSL.Cop, T]

  def prg[M[_]: Monad](dSLTestM: DSLTestM[M]) =
    for {
      i <- dSLTestM.get
      j <- dSLTestM.getSet
      k <- dSLTestM.get
    } yield (i, j)

  type O = Option :&: Bulb
  type ContextO[T] = freek.OnionT[cats.free.Free, DSL.Cop, O, T] //Free[DSL.Cop, O#Layers[T]]

  println(prg[Context](DSLTestM.impl[DSL]).interpret(DSLTestM.interpreter))
  println(prg[ContextO](DSLTestM.implo[DSL, O]).value.interpret(DSLTestM.interpreterOption))



//
//    sealed trait Instruction[T]
//
////  trait DSLTestM[M[_]] {
////    def get: M[Int]
////   // def getH: M[Set[Int]]
////  }
//
//  type DSL = Instruction :|: NilDSL
//  type O = Option :&: Bulb
//
//  Bla().freek[DSL].onionT[O]

  //DSLTestM.get().freek[DSL].onion[O]
 // DSLTestM.getH().freek[DSL].onion[O]

}

//object DSLWithOnionTest {

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

//}
