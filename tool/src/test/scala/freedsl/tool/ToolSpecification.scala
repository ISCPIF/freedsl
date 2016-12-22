package freedsl.tool

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import cats._

object ToolSpecification extends Properties("Tool") {

  property("repeat") = forAll { (size: Int) =>
    (size >= 0 && size < 1000) ==> ((1: Id[Int]).repeat(size).size == size)
  }

}