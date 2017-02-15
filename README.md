# FreeDL


## Overview

FreeDSL is a library for pure composition of side effects, weaving typeclasses on an abstract wrapping type and the Free Monad.

## Examples

### Creating a new DSL

```scala
import freedsl.dsl._

object Random {

  def interpreter(random: util.Random) = new Interpreter {
    def nextDouble(implicit context: Context) = random.nextDouble
    def nextInt(n: Int)(implicit context: Context) = random.nextInt(n)
    def shuffle[A](s: Seq[A])(implicit context: Context) = result(random.shuffle(s))
    def fakeError(implicit context: Context) = failure(FakeError("for some reason there was an error"))
  }

  def interpreter(seed: Long): Interpreter = interpreter(new util.Random(seed))
  
  case class FakeError(cause: String) extends Error

}

@dsl trait Random[M[_]] {
  def nextDouble: M[Double]
  def nextInt(n: Int): M[Int]
  def shuffle[A](s: Seq[A]): M[Seq[A]]
  def fakeError: M[Unit]
}
```

### Using your new DSL

#### Merging interpreters

```scala
import cats._
import cats.implicits._
import freedsl._
import freedsl.random._
import freedsl.util._
import freedsl.log._

import squants.time.TimeConversions._

// Pure functions that describe computations depending on side effects
def randomData[M[_]](implicit randomM: Random[M]): M[Seq[Int]] =
  randomM.shuffle(Seq(1, 2, 2, 3, 3, 3))

def randomSleep[M[_]: Monad](implicit randomM: Random[M], utilM: Util[M], logM: Log[M]): M[Unit] = for {
  s <- randomM.nextInt(10)
  _ <- logM.print(s"Sleeping for $s seconds")
  _ <- utilM.sleep(s seconds)
} yield ()


// Construct the interpreter and a type M along with implicit instances of Random[M], Util[M] and Log[M]
// they are build using the free monad and the freek library

val interpreter = merge(Util.interpreter, Random.interpreter(42), Log.interpreter)
import interpreter.implicits._

def prg =
  for {
    b ← randomData[interpreter.M]
    _ ← randomSleep[interpreter.M]
  } yield b
  

// All the side effects take place here in the interpreter
interpreter.run(prg) match {
  case Right(v) => println(s"This is a success: $v")
  case Left(e) => println(s"OhOh, error: $e")
}
```
#### Merging DSLs

Types can be merged independently from intepreters. For instance:

```scala
val merged = merge(Util, Random, Log)
import merged.implicits._

def prg =
  for {
    b ← randomData[merged.M]
    _ ← randomSleep[merged.M]
  } yield b

  
val interpreter = merge(Util.interpreter, Random.interpreter(42), Log.interpreter)

// All the side effects take place here in the interpreter
interpreter.run(prg) match {
  case Right(v) => println(s"This is a success: $v")
  case Left(e) => println(s"OhOh, error: $e")
}
```


#### Mutli-level merging

DSLs and interpreters can be merged at multiple scala. For instance:

```scala
val partialMerge = merge(Util, Random)
val merged = merge(partialMerge, Log)
import merged.implicits._

def prg =
  for {
    b ← randomData[merged.M]
    _ ← randomSleep[merged.M]
  } yield b

  
val interpreter = merge(Util.interpreter, Random.interpreter(42), Log.interpreter)

// All the side effects take place here in the interpreter
interpreter.run(prg) match {
  case Right(v) => println(s"This is a success: $v")
  case Left(e) => println(s"OhOh, error: $e")
}
```

And for interpreters:

```scala
// Construct the interpreter and a type M along with implicit instances of Random[M], Util[M] and Log[M]
// they are build using the free monad and the freek library

val partialMerge = merge(Util.interpreter, Random.interpreter(42))

// In case several interpreters are provided for a given DSL, the first one in the list is retained
val interpreter = merge(partialMerge, Log.interpreter)
import interpreter.implicits._

def prg =
  for {
    b ← randomData[interpreter.M]
    _ ← randomSleep[interpreter.M]
  } yield b
  

// All the side effects take place here in the interpreter
interpreter.run(prg) match {
  case Right(v) => println(s"This is a success: $v")
  case Left(e) => println(s"OhOh, error: $e")
}
```

## Wrapping errors

To return meaningful errors, it is sometimes necessary to wrap errors produced by side effects:

```scala

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
```

## Getting FreeDSL

FreeDSL is published to [sonatype](https://oss.sonatype.org/).

FreeDSL supports Scala 2.11 and 2.12. If you use SBT, you can
include FreeDSL via the following `build.sbt` snippets:

```scala
// Repository for Freek
resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven")

// For scala 2.12, for 2.11 use Miles Sabin's plugin for type unification.
scalacOptions := Seq("-Ypartial-unification")

def freedslVersion = "0.7"

// pick a particular subproject
libraryDependencies += "fr.iscpif.freedsl" %% "util" % freedslVersion,
libraryDependencies += "fr.iscpif.freedsl" %% "random" % freedslVersion % "test"
```

### Detailed Information

TODO

### Known Issues

DSLs don't suport default parameter value. It raises an error at runtime, such as:
```
java.lang.AbstractMethodError: freedsl.dsl.DSLTest$InterpretationContext$1$MergedDSL$1$$anon$12$$anon$10.defaultValue$default$2()Ljava/lang/String;
```

### Future Work

TODO

### Copyright and License

All code is available to you under the LGPL license, available at
https://www.gnu.org/licenses/lgpl.html and also in the
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
