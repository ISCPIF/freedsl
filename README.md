# FreeDSL


## Overview

FreeDSL is a library for pure composition of side effects weaving typeclasses in higher-kinded types and [Freek]()'s compositions features for the Free Monad.

## Examples

### Creating a new DSL

```scala
import cats._
import cats.implicits._
import freedsl.dsl._

object Random {

  def interpreter(random: util.Random) = new Interpreter[Id] {
    def interpret[_] = {
      case nextDouble() => Right(random.nextDouble)
      case nextInt(n) => Right(random.nextInt(n))
      case shuffle(s) => Right(random.shuffle(s))
    }
  }

  def interpreter(seed: Long): Interpreter[Id] = interpreter(new util.Random(seed))

}

@dsl trait Random[M[_]] {
  def nextDouble: M[Double]
  def nextInt(n: Int): M[Int]
  def shuffle[A](s: Seq[A]): M[Seq[A]]
}
```

### Using your new DSL

```scala
import freek._
import cats._
import cats.implicits._
import freedsl.random._
import freedsl.util._
import freedsl.log._
import concurrent.duration._

// Pure functions that descibe side effects
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
```

## Getting FreeDSL

FreeDSL is published to [sonatype](https://oss.sonatype.org/).

FreeDSL supports Scala 2.11 and 2.12. If you use SBT, you can
include FreeDSL via the following `build.sbt` snippets:

```scala
def freedslVersion = "1.0-SNAPSHOT"

// pick a particular subproject
libraryDependencies += "fr.iscpif.freedsl" %% "util" % freedslVersion,
libraryDependencies += "fr.iscpif.freedsl" %% "random" % freedslVersion % "test"
```

### Detailed Information

TODO

### Known Issues

TODO

### Future Work

TODO

### Copyright and License

All code is available to you under the LGPL license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](COPYING) file.

Copyright Romain Reuillon, 2016

### No Warranty

> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
> NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
> BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
> ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
> CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
> SOFTWARE.
