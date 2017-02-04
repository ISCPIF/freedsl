package freedsl.example

object DSLExample extends App {
  import cats._
  import cats.implicits._
  import freedsl.random._
  import freedsl.system._
  import freedsl.log._
  import squants.time.TimeConversions._

  // Pure functions depending on side effects
  def randomData[M[_]](implicit randomM: Random[M]) =
    randomM.shuffle(Vector(1, 2, 2, 3, 3, 3))

  def randomSleep[M[_]: Monad](implicit randomM: Random[M], utilM: System[M], logM: Log[M]) = for {
    t <- randomM.nextDouble
    s = (t * 10).toInt
    _ <- logM.print(s"Sleeping for $s seconds")
    _ <- utilM.sleep(s seconds)
  } yield ()

  // Construct an appropriate M along with implicit instances of Random[M], Util[M] and Log[M]
  // they are build using the free monad and the freek library
  val intp = freedsl.dsl.merge(Random.interpreter(42), System.interpreter, Log.interpreter)
  import intp.implicits._

  val prg =
    for {
      b ← randomData[intp.M]
      _ ← randomSleep[intp.M]
    } yield b


  // All the side effects take place here in the interpreter
  intp.run(prg) match {
    case Right(v) => println(s"This is a success: $v")
    case Left(e) => println(s"OhOh, error: $e")
  }

}
