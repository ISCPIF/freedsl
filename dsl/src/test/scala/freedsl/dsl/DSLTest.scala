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
  import freek._

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


object PureFreek3 extends App {

  import cats._
  import cats.data._
  import freek._

  sealed trait Instruction[T]
  final case class Get() extends Instruction[Either[String, Int]]


  type I = Instruction :|: NilDSL
  type O = Either[String, ?] :&: Bulb

  def prog =
    for {
      i <- Get().freek[I].onionX1[O]
    } yield i

  def interpreter = new (Instruction ~> Id) {
    def apply[A](a: Instruction[A]) = a match {
      case Get() => Right(1)
    }
  }

  println(prog.value.interpret(interpreter))
}


object PureCats extends App {
  import cats._
  import cats.free._

  sealed trait Instruction[T]
  final case class Get() extends Instruction[Int]
  final case class Get2() extends Instruction[Double]

  def get(): Free[Instruction, Int] = Free.liftF(Get())
  def get2(): Free[Instruction, Double] = Free.liftF(Get2())

  val prg =
    for {
      g <- get()
      g2 <- get2()
    } yield g * g2

  val interpreter = new (Instruction ~> Id) {
    def apply[A](fa: Instruction[A]): Id[A] = fa match {
      case Get() => 1
      case Get2() => 2.0
    }
  }

  println(prg.foldMap(interpreter))

}

object MergeCats extends App {
  import cats._
  //import cats.data._
  import cats.data.Coproduct, cats.free.Inject, cats.free.Free
  
  object DSL1 {
    sealed trait Instruction[T]
    final case class Get() extends Instruction[Int]
    final case class Get2() extends Instruction[Double]

    implicit def dsl1[F[_]](implicit I: Inject[Instruction, F]): DSL1[F] = new DSL1[F]

     val interpreter = new (Instruction ~> Id) {
       def apply[A](fa: Instruction[A]): Id[A] = fa match {
         case Get() => 1
         case Get2() => 2.0
       }
     }
  }

  class DSL1[F[_]](implicit I: Inject[DSL1.Instruction, F]) {
    def get(): Free[F, Int] = Free.inject[DSL1.Instruction, F](DSL1.Get())
    def get2(): Free[F, Double] = Free.inject[DSL1.Instruction, F](DSL1.Get2())
  }

  object DSL2 {
    sealed trait Instruction[T]
    final case class Get() extends Instruction[Int]
    final case class Get2() extends Instruction[Double]

    implicit def dsl2[F[_]](implicit I: Inject[Instruction, F]): DSL2[F] = new DSL2[F]

    val interpreter = new (Instruction ~> Id) {
      def apply[A](fa: Instruction[A]): Id[A] = fa match {
        case Get() => 3
        case Get2() => 4.0
      }
    }
  }

  class DSL2[F[_]](implicit I: Inject[DSL2.Instruction, F]) {
    def get(): Free[F, Int] = Free.inject[DSL2.Instruction, F](DSL2.Get())
    def get2(): Free[F, Double] = Free.inject[DSL2.Instruction, F](DSL2.Get2())
  }

  object DSL3 {
    sealed trait Instruction[T]
    final case class Get() extends Instruction[Int]
    final case class Get2() extends Instruction[Double]

    implicit def dsl3[F[_]](implicit I: Inject[Instruction, F]): DSL3[F] = new DSL3[F]

    val interpreter = new (Instruction ~> Id) {
      def apply[A](fa: Instruction[A]): Id[A] = fa match {
        case Get() => 4
        case Get2() => 5.0
      }
    }
  }

  class DSL3[F[_]](implicit I: Inject[DSL3.Instruction, F]) {
    def get(): Free[F, Int] = Free.inject[DSL3.Instruction, F](DSL3.Get())
    def get2(): Free[F, Double] = Free.inject[DSL3.Instruction, F](DSL3.Get2())
  }

  type MergedDSL[A] = Coproduct[DSL1.Instruction,  Coproduct[DSL2.Instruction, DSL3.Instruction, ?], A]

  def prg(implicit dsl1: DSL1[MergedDSL], dsl2: DSL2[MergedDSL], dsl3: DSL3[MergedDSL]) =
    for {
      g <- dsl1.get()
      g2 <- dsl1.get2()
      sg <- dsl2.get()
      sg2 <- dsl2.get2()
    } yield g * g2 * sg * sg2

  val interpreter: MergedDSL ~> Id = DSL1.interpreter or (DSL2.interpreter or DSL3.interpreter)

  println(prg.foldMap(interpreter))

//  val interpreter = new (Instruction ~> Id) {
//    def apply[A](fa: Instruction[A]): Id[A] = fa match {
//      case Get() => 1
//      case Get2() => 2.0
//    }
//  }
//
//  println(prg.foldMap(interpreter))

}

object DSLTest extends App {

  object DSLTest1M {
    def interpreter = new Interpreter {
      def get(implicit context: Context) = success(1)
      def set(i: Double)(implicit context: Context) = success(2)
      def setF(f: () => Int)(implicit context: Context) = success(f())
      def set(i: Int)(implicit j: Int, context: Context) = success(i * j)
      def param[A](a: A)(implicit context: Context) = success(a)
      def defaultValue(i: Int, s: String)(implicit context: Context) = success(())
      def fails(implicit context: Context) = failure(ItFailed("booo"))
      override def terminate(implicit context: Context) = success(())
    }

    case class FileNotFound(s: String) extends Error
    case class ItFailed(s: String) extends Error
  }

  @dsl trait DSLTest1M[M[_]] {
    def get: M[Int]
    def setF(f: () => Int): M[Int]
    def set(i: Double): M[Int]
    def set(i: Int)(implicit j: Int): M[Int]
    def param[A](a: A): M[A]
    def defaultValue(i: Int, s: String = ""): M[Unit]
    def fails: M[Unit]
    def concreteMethod = 9
    def test(i: Int = 9) = i
  }

  object DSLTest2M {
    def interpreter = new Interpreter {
      def get(implicit context: Context) = success("dsl2 is nice")
    }
  }

  trait AbstractDSL2[M[_], S] {
    def get: M[S]
  }

  @dsl trait DSLTest2M[M[_]] extends AbstractDSL2[M, String] {
    def get: M[String]
  }


  object DSLTest3M {
    def interpreter = new Interpreter {
      def test(implicit context: Context) = success(())
    }
  }

  @dsl trait DSLTest3M[M[_]] {
    def test: M[Unit]
  }

  import cats._
  import cats.implicits._

  def prg[M[_]: Monad](implicit dslTest1M: DSLTest1M[M], dslTest2M: AbstractDSL2[M, String]) =
    for {
      _ <- dslTest1M.defaultValue(9)
      i <- dslTest1M.get
      k <- dslTest1M.get
      l <- dslTest2M.get
      // FIXME for some non obvious reson it doesn't work with default parameters values => Runtime Error
      // _ <- dslTest1M.defaultValue(9)
      // _ <- dslTest1M.fails
    } yield (i, k, l)

  val intp = merge(DSLTest1M.interpreter, DSLTest2M.interpreter, DSLTest3M.interpreter)
  import intp.implicits._

  println(intp.run(prg[intp.M]))

}


object MultiLevelMerge extends App {
  import cats._
  import cats.implicits._

  object DSLTest1M {
    def interpreter = new Interpreter {
      def get(implicit context: Context) = success("dsl1 is nice")
    }
  }

  object DSLTest2M {
    def interpreter = new Interpreter {
      def get(implicit context: Context) = success("dsl2 is nice")
    }
  }

  object DSLTest3M {
    def interpreter = new Interpreter {
      def get(implicit context: Context) = success("dsl3 is nice")
    }
  }

  @dsl trait DSLTest1M[M[_]] {
    def get: M[String]
  }

  @dsl trait DSLTest2M[M[_]] {
    def get: M[String]
  }

  @dsl trait DSLTest3M[M[_]] {
    def get: M[String]
  }

  def prg[M[_]: Monad: DSLTest1M: DSLTest2M: DSLTest3M] = for {
    _ <- DSLTest1M[M].get
    _ <- DSLTest3M[M].get
  } yield ()

  def withInterpreters = {
    val merged1 = merge(DSLTest1M.interpreter, DSLTest2M.interpreter)
    val merged3 = merge(merged1, DSLTest3M.interpreter)

    import merged3.implicits._
    println(merged3.run(prg[merged3.M]))
  }


  def withDSL = {
    val merged1 = merge(DSLTest1M, DSLTest2M)
    val merged2 = merge(DSLTest3M, DSLTest2M)
    val merged3 = merge(merged1, merged2, DSLTest3M)

    import merged3._
    import merged3.implicits._

    val intp = merge(DSLTest1M.interpreter, DSLTest2M.interpreter, DSLTest3M.interpreter)
    println(intp.run(prg[M]))
  }

  withInterpreters
  withDSL
}

object ErrorWrapping extends App {

  object DSLTest1M {
    def interpreter = new Interpreter {
      def get(implicit context: Context) = failure(DSL1Error("Boooh"))
    }

    case class DSL1Error(s: String) extends Error
  }

  @dsl trait DSLTest1M[M[_]] {
    def get: M[String]
  }

  case class WrapError(e: Error) extends Error

  def prg[M[_]](implicit dSLTest1M: DSLTest1M[M], errorWrapping: ErrorWrapping[M]) = errorWrapping.wrap(e => WrapError(e)) {
    dSLTest1M.get
  }

  val intp = merge(DSLTest1M.interpreter)
  import intp.implicits._

  println(intp.run(prg[intp.M]))

}
