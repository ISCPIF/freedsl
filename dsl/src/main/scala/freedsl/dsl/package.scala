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
        q"case class ${TypeName(opTerm(func))}(..${params}) extends ${instructionName}[${func.tpt.children.drop(1).head}]"
      }


      val caseClasses = funcs.map(c => generateCaseClass(c))

      val generateFreekImpl =
        (func: DefDef) => {
          val params = func.vparamss.flatMap(_.map(p => q"${p.name.toTermName}: ${p.tpt}"))
          q"def ${func.name}(..${params}) = $name.${TermName(opTerm(func))}(..${params}).freek[DSL0]"
        }

      val generateFreekoImpl =
        (func: DefDef) => {
          val params = func.vparamss.flatMap(_.map(p => q"${p.name.toTermName}: ${p.tpt}"))
          q"def ${func.name}(..${params}) = $name.${TermName(opTerm(func))}(..${params}).freek[DSL0].onion[O0]"
        }

      def implDefs(transform: DefDef => Tree) = funcs.map(transform)


      val modifiedCompanion = q"""
        $mods object $name extends ..$bases {
           sealed trait ${instructionName}[T]
           ..${caseClasses}

           type DSL = freek.:|:[${instructionName}, freek.NilDSL]
           val DSL = freek.DSL.Make[DSL]

           type Instruction[T] = ${instructionName}[T]

           trait Interpreter[T[_]] extends cats.~>[${instructionName}, T] {
             def interpret[A]: (${instructionName}[A] => T[A])
             def apply[A](f: ${instructionName}[A]) = interpret[A](f)
           }

           def impl[DSL0 <: freek.DSL](implicit subDSL: freek.SubDSL1[${instructionName}, DSL0]) = new ${clazz.name}[({type l[A] = cats.free.Free[subDSL.Cop, A]})#l] {
             import freek._
             ..${implDefs(generateFreekImpl)}
           }

           ..$body
        }
      """

      c.Expr(q"""
        $clazz
        $modifiedCompanion""")
    }

//    def implo[DSL0 <: freek.DSL, O0 <: freek.Onion: freek.Pointer: freek.Mapper: freek.Binder: freek.Traverser](implicit subDSL: freek.SubDSL1[${traitName}, DSL0]) = new ${clazz.name}[({type l[A] = freek.OnionT[cats.free.Free, subDSL.Cop, O0, A]})#l] {
//      import freek._
//      ..${implDefs(generateFreekoImpl)}
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



}
