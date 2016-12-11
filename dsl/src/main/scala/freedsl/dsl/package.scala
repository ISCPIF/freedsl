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

      def collect(t: List[Tree]): List[DefDef] =
        t.collect {
          case m: DefDef
            if !m.mods.hasFlag(Flag.PRIVATE) &&
              !m.mods.hasFlag(Flag.PROTECTED) &&
              m.mods.hasFlag(Flag.DEFERRED) => m
        }

      val funcs = collect(clazz.impl.children)

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
           import cats._

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
      case other :: Nil =>applicationConditionError
    }

  }

  class dsl extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro dsl_impl
  }

  def context_impl(c: Context)(objects: c.Expr[freedsl.dsl.DSLObject]*): c.Expr[Any] = {
    import c.universe._

    val I = objects.map(o => tq"${o}.I").foldRight(tq"freek.NilDSL": Tree)((o1, o2) => tq"freek.:|:[$o1, $o2]": Tree)
    val O = objects.map(o => tq"${o}.O").foldRight(tq"freek.Bulb": Tree)((o1, o2) => tq"freek.:&:[$o1, $o2]": Tree)

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
            ..${funcs.map(f => generateFreekoImpl(f))}
          }
        }"""
      typeClass.typeParams.size match {
        case 1 => Some (q"implicit def ${TermName (c.freshName ("impl") )} = $implem")
        case 0 => None
      }
    }

    monocle.std.either.stdRight[Int, Either[Int, Int]] composePrism monocle.std.either.stdRight[Int, Int] composePrism monocle.Prism.id[Int]

    //   val resType = objects.foldRight(tq"T": Tree) { (o1, c) => tq"Either[${o1}.Error,$c]": Tree }

    val (resType, resLens) = objects.foldRight((tq"T": Tree, q"monocle.Prism.id[T]": Tree)) {
      case (o1, (t, c)) =>
        val curType = tq"Either[${o1}.Error,$t]"
        (curType: Tree, q"monocle.std.either.stdRight[${o1}.Error, $t] composePrism $c")
    }


    def getError(level: Int) = {
      val name = TermName(s"error$level") //c.freshName("getError")
      def getter = (0 until level).foldLeft(q"Some(v)": Tree)((e,_) => q"$e.flatMap(_.right.toOption)": Tree)
      q"def ${name}[T](v: $resType): Option[${objects(level)}.Error] = $getter.flatMap(_.left.toOption)"
    }

    def anyError(levels: Int) = {
      def errors =
        (0 until levels).foldLeft(q"None": Tree)((e, i) => q"$e orElse ${TermName(s"error$i")}(v)")

      q"def error[T](v: $resType) = $errors"
    }


    val res = c.Expr(
      q"""class Context { self =>
           import freek._
           import cats._
           import cats.implicits._

           type I = $I
           type O = $O

           val DSLInstance = freek.DSL.Make[I]

           def valueLens[T] = $resLens
           def value[T](t: $resType): Option[T] = valueLens.getOption(t)

           ..${(0 until objects.size).map(i => getError(i))}
           ${anyError(objects.size)}

           $mType

           import freek._
           ..${objects.flatMap(o => implicitFunction(o))}

           def unwrapResult[T](t: $resType) =
            (value(t), error(t)) match {
              case (Some(v), _) => Right(v)
              case (_, Some(e)) => Left(e)
              case (None, None) => sys.error("Result is either a value or an error")
            }

            // Don't exactly know why but it seem not possible to abstact over id
            def result[T](mt: M[T], interpreter: freek.Interpreter[DSLInstance.Cop, Id]) = {
              val res = mt.value.interpret(interpreter)
              implicitly[cats.Monad[Id]].map(res) { r => unwrapResult(r) }
            }
         }
         new Context
        """)

    //println(res)
    res
  }

  def merge(objects: freedsl.dsl.DSLObject*) = macro context_impl



}
