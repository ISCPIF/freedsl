package freedsl.example

object DSLExample extends App {
  import cats._
  import cats.implicits._
  import freestyle._
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

  def prg[M[_]: Monad](implicit randomM: Random[M], utilM: System[M], logM: Log[M]) =
    for {
      b ← randomData[M]
      _ ← randomSleep[M]
    } yield b

  implicit val randomInterpreter = RandomInterpreter(42)
  implicit val systemInterpreter = SystemInterpreter()
  implicit val logInterpreter = LogInterpreter()

  println(prg[freedsl.dsl.Evaluated])

}
