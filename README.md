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
import cats.implicits._
import freedsl.random._
import freedsl.util._

val c = freedsl.dsl.merge(Random, Util)
import c._

def randomData[M[_]](implicit randomM: Random[M]) = randomM.shuffle(Seq(1, 2, 2, 3, 3, 3))

val prg =
  for {
    a ← nextInt[M]()
    _ ← implicitly[Util[M]].sleep(2 second)
    b ← nextInt[M]()
  } yield s"""$a * $b  = ${a*b}"""


val interpreter =
    Util.interpreter :&:
    Random.interpreter(42)

println(result.getOption(prg.value.interpret(interpreter)))
```

## Getting FreeDSL

Sortilege is published to [sonatype](https://oss.sonatype.org/).

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
