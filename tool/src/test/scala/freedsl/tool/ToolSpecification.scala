package freedsl.tool

import org.scalatest._

class ToolSpecification extends FlatSpec with Matchers {

  type OneArg[A] = cats.data.State[Int, A]

  def increment(): OneArg[Int] = {
    import cats.data._
    State[Int, Int](i => (i + 1, i + 1))
  }

  "until" should "call the monad until the test gets false" in {
    increment.until((x: Int) => x >= 10).run(0).value should equal((10, 10))
  }

  "repeat" should "call the monad 10 times" in {
    increment.repeat(10).run(0).value._2 should equal((1 to 10).toVector)
  }

  "repeat" should "generate a random vector" in {
    import cats._
    import cats.implicits._
    import freedsl.random._
    import freedsl.dsl._

    implicit val interpreter = RandomInterpreter(42)

    def vec[M[_]: Monad: Random](size: Int) =
      repeat(freedsl.random.Random[M].nextDouble(), size)

    vec[DSL](100).eval.distinct.size shouldNot equal(1)
  }

}