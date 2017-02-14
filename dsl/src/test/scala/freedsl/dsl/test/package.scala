package freedsl.dsl

package object test {

  @dsl trait Test[M[_]] {
    def get: M[Int]
  }

}
