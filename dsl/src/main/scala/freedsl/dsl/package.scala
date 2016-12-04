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
package freedsl

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

package object dsl {

  trait DSLObject {
    type I[_]
    type O[_]
  }

  trait DSLError

  def dsl_impl(c: Context)(annottees: c.Expr[Any]*) = {
    import c.universe._

    def generateCompanion(clazz: ClassDef, comp: Tree) = {

      val instructionName = TypeName(c.freshName("Instruction"))
      val q"$mods object $name extends ..$bases { ..$body }" = comp

      val funcs = clazz.impl.children.collect {
        case m: DefDef if !m.mods.hasFlag(Flag.PRIVATE) && !m.mods.hasFlag(Flag.PROTECTED) => m
      }

      def opTerm(func: DefDef) = func.name.toString

      def generateCaseClass(func: DefDef) = {
        val params = func.vparamss.flatMap(_.map(p => q"${p.name.toTermName}: ${p.tpt}"))
        //FIXME Issue proper error
        q"case class ${TypeName(opTerm(func))}(..${params}) extends ${instructionName}[Either[Error, ${func.tpt.children.drop(1).head}]]"
      }

      val caseClasses = funcs.map(c => generateCaseClass(c))
      val dslObjectType = weakTypeOf[DSLObject]
      val dslErrorType = weakTypeOf[DSLError]

      val modifiedCompanion = q"""
        $mods object $name extends ..$bases with $dslObjectType {
           type TypeClass[M[_]] = ${clazz.name}[M]

           sealed trait ${instructionName}[T]
           ..${caseClasses}

           sealed trait Error extends $dslErrorType

           type O[T] = Either[Error, T]
           type I[T] = ${instructionName}[T]

           trait Interpreter[T[_]] extends cats.~>[I, T] {
             def interpret[A]: (I[A] => T[A])
             def apply[A](f: I[A]) = interpret[A](f)
           }

           ..$body
        }
      """

      println(modifiedCompanion)

      c.Expr(q"""
        $clazz
        $modifiedCompanion""")
    }

//    val generateFreekImpl =
//      (func: DefDef) => {
//        val params = func.vparamss.flatMap(_.map(p => q"${p.name.toTermName}: ${p.tpt}"))
//        q"def ${func.name}(..${params}) = $name.${TermName(opTerm(func))}(..${params}).freek[DSL0]"
//      }
//
//    val generateFreekoImpl =
//      (func: DefDef) => {
//        val params = func.vparamss.flatMap(_.map(p => q"${p.name.toTermName}: ${p.tpt}"))
//        val returnType = func.tpt.children.drop(1).head
//
//        q"def ${func.name}(..${params}) = $name.${TermName(opTerm(func))}(..${params}).freek[DSL0].onionX1[O0]"
//      }
//    def impl[DSL0 <: freek.DSL](implicit subDSL: freek.SubDSL1[${instructionName}, DSL0]) = new ${clazz.name}[({type l[A] = cats.free.Free[subDSL.Cop, A]})#l] {
//      import freek._
//      ..${funcs.map(generateFreekImpl)}
//    }

//    def implo[DSL0 <: freek.DSL, O0 <: freek.Onion](implicit subDSL: freek.SubDSL1[${instructionName}, DSL0], lift: Lifter[({type l[A] = Either[Error, A]})#l, O0]) = new ${clazz.name}[({type l[A] = freek.OnionT[cats.free.Free, subDSL.Cop, O0, A]})#l] {
//      import freek._
//      import cats._
//      import cats.implicits._
//
//      ..${funcs.map(generateFreekoImpl)}
//    }

    def modify(typeClass: ClassDef, companion: Option[ModuleDef]) = generateCompanion(typeClass, companion.getOrElse(q"object ${typeClass.name.toTermName} {}"))

    annottees.map(_.tree) match {
      case (typeClass: ClassDef) :: Nil => modify(typeClass, None)
      case (typeClass: ClassDef) :: (companion: ModuleDef) :: Nil => modify(typeClass, Some(companion))
      case other :: Nil =>
        c.abort(
          c.enclosingPosition,
          "@typeclass can only be applied to traits or abstract classes that take 1 type parameter which is either a proper type or a type constructor"
        )
    }

  }

  class dsl extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro dsl_impl
  }

  def dslImpl_Impl[T[_[_]], I: c.WeakTypeTag, O: c.WeakTypeTag](c: Context)(implicit tTypeTag: c.WeakTypeTag[T[Nothing]]): c.Expr[Any] = {
    import c.universe._

    val tType = weakTypeOf[T[Nothing]]
    val dslType = weakTypeOf[I]
    val oType = weakTypeOf[O]

    val tTypeName =  tType.typeSymbol.asType.name

    val funcs: List[MethodSymbol] = tType.decls.collect { case s: MethodSymbol ⇒ s }.toList

    val generateFreekoImpl =
      (func: MethodSymbol) => {
        val params = func.paramLists.flatMap(_.map(p => q"${p.name.toTermName}: ${p.typeSignature}"))
        val companion = tTypeName.toTermName
        q"def ${func.name}(..${params}) = ${companion}.${func.name}(..${params}).freek[${dslType}].onionX1[${oType}]"
      }

    val implem = q"""{
      val DSLInstance = freek.DSL.Make[I]

      new $tTypeName[({type l[T] = freek.OnionT[cats.free.Free, DSLInstance.Cop, ${oType}, T]})#l] {
        import freek._
        ..${funcs.map(generateFreekoImpl)}
      }
    }"""

    c.Expr(implem)
  }

  def dslImpl[T[_[_]], I, O] = macro dslImpl_Impl[T, I, O]

  def context_impl(c: Context)(objects: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val I = objects.map(o => tq"${o}.I").foldRight(tq"freek.NilDSL": Tree)((o1, o2) => tq"$o1 :|: $o2": Tree)
    val O = objects.map(o => tq"${o}.O").foldRight(tq"freek.Bulb": Tree)((o1, o2) => tq"$o1 :&: $o2": Tree)

    def implicitFunction(o: Tree) =
      q"implicit def ${TermName(c.freshName("impl"))} = dslImpl[${o}.TypeClass, I, O]"

//    def wrapIn(n: Int, w: String => String, s: String): String =
//      n match {
//        case 0 => s
//        case n => w(wrapIn(n - 1, w, s))
//      }
//
//    def cases(level: Int): List[String] =
//      (level to 0 by -1).toList flatMap { l =>
//
//        List(
//          wrapIn(l, w => s"Right($w)", "Left(x)"),
//          wrapIn(l, w => s"Right($w)", "Right(x)")
//        )
//      } map { w => s"case $w => x" }
//
//    def mutliEither(level: Int): String = wrapIn(level, w => s"Either[freedsl.dsl.DSLError, $w]", "T")
//
//    println(cases(objects.size))
//    println(mutliEither(objects.size))

    val res = c.Expr(
      q"""new { self =>

           type I = $I
           type O = $O
           val DSLInstance = freek.DSL.Make[I]
           type M[T] = freek.OnionT[cats.free.Free, DSLInstance.Cop, O, T]
           ..${objects.map(o => implicitFunction(tq"${o}"))}
         }""")
println(res)
    res
  }


//  type I = $I
//  type O = $O

  def merge(objects: Any*) = macro context_impl

//  def context[I: c.WeakTypeTag, O: c.WeakTypeTag](c: Context) = {
//    import c.universe._
//
//    q"""{
//        val DSLInstance = freek.DSL.Make[I]
//        type Context[T] = freek.OnionT[cats.free.Free, DSLInstance.Cop, O, T]
//        }"""
//  }

}
