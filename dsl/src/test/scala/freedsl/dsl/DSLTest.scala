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



object PureFreek extends App {

  import cats._
  import cats.data._
  import freek._
  import cats.implicits._

  sealed trait Instruction[T]
  final case class Get() extends Instruction[Int]
  final case class GetSet() extends Instruction[Set[Int]]
  final case class GetMap() extends Instruction[Map[Int, Int]]

  type DSL = Instruction :|: NilDSL
  type O = Option :&: Bulb

  val prog =
    for {
      i <- Get().freek[DSL].onionT[O]
      j <- GetSet().freek[DSL].onion[O]
      k<- GetMap().freek[DSL].onion[O]
    } yield (i, j, k)


  def interpreter = new (Instruction ~> Option) {
    def apply[A](a: Instruction[A]) = a match {
      case Get() => Some(1)
      case GetSet() => Some(Set(1))
      case GetMap() => Some(Map(1 -> 1))
    }
  }

  println(prog.value.interpret(interpreter))
}


object PureFreek2 extends App {

  import cats._
  import cats.data._
  import freek._
  import cats.implicits._

  sealed trait Instruction[T]
  final case class Get() extends Instruction[Option[Int]]
  final case class GetSet() extends Instruction[Option[Set[Int]]]
  final case class GetMap() extends Instruction[Option[Map[Int, Int]]]
  final case class GetMap2() extends Instruction[Ior[String, Map[Int, Int]]]
  final case class ParametricGet[A](a: A) extends Instruction[Option[A]]


  type I = Instruction :|: NilDSL
  type O = Ior[String, ?] :&: Option :&: Bulb

  def prog[T](t: T) =
    for {
      i <- Get().freek[I].onionX1[O]
      j <- GetSet().freek[I].onionX1[O]
      k <- GetMap().freek[I].onionX1[O]
      l <- GetMap2().freek[I].onionX1[O]
      m <- ParametricGet(t).freek[I].onionX1[O]
    } yield (i, j, k, l, m)

  def interpreter = new (Instruction ~> Id) {
    def apply[A](a: Instruction[A]) = a match {
      case Get() => Some(1)
      case GetSet() => Some(Set(1))
      case GetMap() => Some(Map(1 -> 1))
      case GetMap2() => Ior.right(Map(8 -> 9))
      case ParametricGet(a) => Some(a)
    }
  }

  println(prog(9).value.interpret(interpreter))
}


object DSLTest extends App {

  object DSLTest1M {
    def interpreter = new Interpreter[Id] {
      def interpret[_] = {
        case get() => Right(1)
        case getSet() => Right(Set(1))
        case option() => Right(Some("cool"))
        case set(i) => Right(i)
        case param(a) => Right(a)
        case fails() => Left(ItFailed("Boooo"))
      }
    }

    case class FileNotFound(s: String) extends Error
    case class ItFailed(s: String) extends Error
  }

  @dsl trait DSLTest1M[M[_]] {
    def get: M[Int]
    def getSet: M[Set[Int]]
    def option: M[Option[String]]
    def set(i: Int): M[Int]
    def param[A](a: A): M[A]
    def fails: M[Unit]
  }

  object DSLTest2M {
    def interpreter = new Interpreter[Id] {
      def interpret[_] = {
        case get() => Right("dsl2 is nice")
      }
    }
  }

  @dsl trait DSLTest2M[M[_]] {
    def get: M[String]
  }


  import cats._
  import cats.implicits._
  import freek._

  def prg[M[_]: Monad](implicit dslTest1M: DSLTest1M[M], dslTest2M: DSLTest2M[M]) =
    for {
      i <- dslTest1M.get
      j <- dslTest1M.getSet
      k <- dslTest1M.get
      l <- dslTest2M.get
      //_ <- dslTest1M.fails
      o <- dslTest1M.option
    } yield (i, j, k, l, o)

  val c = merge(DSLTest1M, DSLTest2M)
  import c._
  val interpreter = DSLTest1M.interpreter :&: DSLTest2M.interpreter
  val res = prg[M].value.interpret(interpreter)

  result(res) match {
    case Right(v) => println(v)
    case Left(e) => println("Error: " + e)
  }

}

