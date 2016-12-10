package freedsl.example

object DSLExample extends App {
  import freek._
  import cats._
  import cats.implicits._
  import freedsl.random._
  import freedsl.util._
  import freedsl.log._
  import concurrent.duration._

  // Pure functions depending on side effects
  def randomData[M[_]](implicit randomM: Random[M]): M[Seq[Int]] =
    randomM.shuffle(Seq(1, 2, 2, 3, 3, 3))

  def randomSleep[M[_]: Monad](implicit randomM: Random[M], utilM: Util[M], logM: Log[M]): M[Unit] = for {
    t <- randomM.nextDouble
    s = (t * 10).toInt
    _ <- logM.print(s"Sleeping for $s seconds")
    _ <- utilM.sleep(s seconds)
  } yield ()

  // Construct an appropriate M along with implicit instances of Random[M], Util[M] and Log[M]
  // they are build using the free monad and the freek library
  val c = freedsl.dsl.merge(Random, Util, Log)
  import c._

  val prg =
    for {
      b ← randomData[M]
      _ ← randomSleep[M]
    } yield b

  // Construct the interpreter for the program
  val interpreter =
    Util.interpreter :&:
      Random.interpreter(42) :&:
      Log.interpreter

  // All the side effects take place here in the interpreter
  result(prg.value.interpret(interpreter)) match {
    case Right(v) => println(s"This is a success: $v")
    case Left(e) => println(s"OhOh, error: $e")
  }

}
