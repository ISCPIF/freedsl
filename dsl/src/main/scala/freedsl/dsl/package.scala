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

      val funcs =
        clazz.impl.children.collect {
          case m: DefDef
            if !m.mods.hasFlag(Flag.PRIVATE) &&
              !m.mods.hasFlag(Flag.PROTECTED) &&
              m.mods.hasFlag(Flag.DEFERRED) => m
        }

      def opTerm(func: DefDef) = func.name.toString

      def generateCaseClass(func: DefDef) = {
        val params = func.vparamss.flatMap(_.map(p => q"${p.name.toTermName}: ${p.tpt}"))
        q"case class ${TypeName(opTerm(func))}[..${func.tparams}](..${params}) extends ${instructionName}[Either[Error, ${func.tpt.children.drop(1).head}]]"
      }

      val caseClasses = funcs.map(c => generateCaseClass(c))
      val dslObjectType = weakTypeOf[DSLObject]
      val dslErrorType = weakTypeOf[DSLError]

      val modifiedCompanion = q"""
        $mods object $name extends ..$bases with $dslObjectType {
           type TypeClass[..${clazz.tparams}] = ${clazz.name}[..${clazz.tparams.map(_.name)}]

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

      c.Expr(q"""
        $clazz
        $modifiedCompanion""")
    }

    def modify(typeClass: ClassDef, companion: Option[ModuleDef]) = generateCompanion(typeClass, companion.getOrElse(q"object ${typeClass.name.toTermName} {}"))

    def applicationConditionError =
       c.abort(
         c.enclosingPosition,
         "@dsl can only be applied to traits or abstract classes that take 1 type parameter which is either a proper type or a type constructor"
       )

    def check(classDef: ClassDef): Option[Nothing] =
      classDef.tparams match {
        case List(p) =>
          p.tparams match {
            case List(pp) => None
            case _ => applicationConditionError
          }
        case _ => applicationConditionError
      }

    annottees.map(_.tree) match {
      case (typeClass: ClassDef) :: Nil => check(typeClass) getOrElse modify(typeClass, None)
      case (typeClass: ClassDef) :: (companion: ModuleDef) :: Nil => check(typeClass) getOrElse modify(typeClass, Some(companion))
      case other :: Nil => applicationConditionError
    }

  }

  class dsl extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro dsl_impl
  }

  def context_impl(c: Context)(objects: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val I = objects.map(o => tq"${o}.I").foldRight(tq"freek.NilDSL": Tree)((o1, o2) => tq"$o1 :|: $o2": Tree)
    val O = objects.map(o => tq"${o}.O").foldRight(tq"freek.Bulb": Tree)((o1, o2) => tq"$o1 :&: $o2": Tree)

    val mType = q"type M[T] = freek.OnionT[cats.free.Free, DSLInstance.Cop, O, T]"

    def implicitFunction(o: c.Expr[Any]) = {
      val typeClass = q"${o}".symbol.asModule.typeSignature.members.collect { case sym: TypeSymbol if sym.name == TypeName("TypeClass") => sym}.head
      val funcs: List[MethodSymbol] = typeClass.typeSignature.decls.collect { case s: MethodSymbol ⇒ s }.toList

      def generateFreekoImpl(m: MethodSymbol) = {
        val typeParams = m.typeParams.map(t => internal.typeDef(t))
        val paramss = m.paramLists.map(_.map(p => internal.valDef(p)))
        val returns = TypeTree(m.returnType)
        val paramValues = paramss.flatMap(_.map(p => q"${p.name.toTermName}"))

        q"def ${m.name}[..${typeParams}](...${paramss}): M[..${returns.tpe.typeArgs}] = { ${o}.${m.name}(..${paramValues}).freek[I].onionX1[O] }"
      }

      val implem = q"""{
        new ${o}.TypeClass[M] {
            import freek._
            ..${funcs.map(f => generateFreekoImpl(f))}
          }
        }"""
      typeClass.typeParams.size match {
        case 1 => Some (q"implicit def ${TermName (c.freshName ("impl") )} = $implem")
        case 0 => None
      }
    }


    val res = c.Expr(
      q"""new { self =>
           type I = $I
           type O = $O
           val DSLInstance = freek.DSL.Make[I]
           $mType
           import  freek._
           ..${objects.flatMap(o => implicitFunction(o))}
         }""")

    //println(res)
    res
  }

  def merge(objects: Any*) = macro context_impl



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
}
