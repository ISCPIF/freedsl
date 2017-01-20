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

import java.util.UUID

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

package object dsl extends
  cats.instances.AllInstances {

  trait DSLObjectIdentifier

  sealed trait MergeableDSLObject
  trait DSLObject extends MergeableDSLObject {
    type I[_]
    type O[_]
  }

  trait MergedDSLObject extends MergeableDSLObject

  trait Error

  sealed trait MergeableDSLInterpreter
  trait DSLInterpreter extends MergeableDSLInterpreter {
    def terminate: Either[Error, Unit] = Right(())
  }
  trait MergedDSLInterpreter extends MergeableDSLInterpreter


  def dsl_impl(c: Context)(annottees: c.Expr[Any]*) = {
    import c.universe._

    def generateCompanion(clazz: ClassDef, comp: Tree) = {
      val instructionName = TypeName(c.freshName("Instruction"))

      val q"$mods object $name extends ..$bases { ..$body }" = comp
      val q"$cmods trait $ctpname[..$ctparams] extends { ..$cearlydefns } with ..$cparents { $cself => ..$cstats }" = clazz

      def abstractMethod(m: DefDef) =
        !m.mods.hasFlag(Flag.PRIVATE) &&
          !m.mods.hasFlag(Flag.PROTECTED) &&
          m.mods.hasFlag(Flag.DEFERRED)

      def collect(t: cstats.type) =
        t.collect { case m: DefDef if abstractMethod(m) => m }

      def opTerm(func: DefDef) = func.name.toString

      def generateCaseClass(func: DefDef) = {
        val params = func.vparamss.flatMap(_.map(p => q"${p.name.toTermName}: ${p.tpt}"))
        q"case class ${TypeName(opTerm(func))}[..${func.tparams}](..${params}) extends ${instructionName}[Either[Error, ${func.tpt.children.drop(1).head}]]"
      }

      val caseClasses = collect(cstats).map(c => generateCaseClass(c))
      val dslObjectType = weakTypeOf[DSLObject]
      val dslErrorType = weakTypeOf[freedsl.dsl.Error]
      val dslInterpreterType = weakTypeOf[freedsl.dsl.DSLInterpreter]
      val dslObjectIdentifierType = weakTypeOf[freedsl.dsl.DSLObjectIdentifier]

      val objectIdentifier = UUID.randomUUID().toString

      val modifiedCompanion = q"""
        $mods object $name extends ..$bases with $dslObjectType { comp =>
           import cats._

           type ${TypeName(objectIdentifier)} = $dslObjectIdentifierType

           type TypeClass[..${clazz.tparams}] = ${clazz.name}[..${clazz.tparams.map(_.name)}]

           sealed trait ${instructionName}[T]
           ..${caseClasses}

           type Error = $dslErrorType

           type I[T] = ${instructionName}[T]
           type O[T] = Either[Error, T]

           trait Interpreter[T[_]] extends cats.~>[I, T] with $dslInterpreterType {
             val companion: $name.type = comp
             type ${TypeName(objectIdentifier)} = $dslObjectIdentifierType
             def interpret[A]: (I[A] => T[A])
             def apply[A](f: I[A]) = interpret(f)
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


  def extractObjectIdentifier(c: Context)(t: c.universe.Tree) = {
    val dslObjectIndentifierType = c.universe.weakTypeOf[freedsl.dsl.DSLObjectIdentifier]
    t.symbol.typeSignature.finalResultType.members.find(_.typeSignature.finalResultType <:< dslObjectIndentifierType).get.name.toString
  }


  def context_impl(c: Context)(objects: c.Expr[freedsl.dsl.MergeableDSLObject]*): c.Expr[freedsl.dsl.MergedDSLObject] = {

    import c.universe._


    def mergeDSLObjects(objects: Seq[c.Expr[freedsl.dsl.DSLObject]]): c.Expr[freedsl.dsl.MergedDSLObject] = {

      val uniqObjects = objects.map(o => extractObjectIdentifier(c)(o.tree) -> o).toMap.values.toSeq
      val sortedObjects = uniqObjects.sortBy(o => extractObjectIdentifier(c)(o.tree))

      val I = sortedObjects.map(o => tq"${o}.I").foldRight(tq"freek.NilDSL": Tree)((o1, o2) => tq"freek.:|:[$o1, $o2]": Tree)

      val mType = q"type M[T] = freek.OnionT[cats.free.Free, DSLInstance.Cop, O, T]"

      def implicitFunction(o: c.Expr[Any]) = {
        val typeClass = {
          def symbol = q"${o}".symbol

          def members =
            if (symbol.isModule) symbol.asModule.typeSignature.members
            else symbol.typeSignature.members

          members.collect { case sym: TypeSymbol if sym.name == TypeName("TypeClass") => sym }.head
        }
        val funcs: List[MethodSymbol] = typeClass.typeSignature.decls.collect { case s: MethodSymbol if s.isAbstract && s.isPublic => s }.toList

        def generateFreekoImpl(m: MethodSymbol) = {
          val typeParams = m.typeParams.map(t => internal.typeDef(t))
          val paramss = m.paramLists.map(_.map(p => internal.valDef(p)))
          val returns = TypeTree(m.returnType)
          val paramValues = paramss.flatMap(_.map(p => q"${p.name.toTermName}"))

          q"def ${m.name}[..${typeParams}](...${paramss}): M[..${returns.tpe.typeArgs}] = { ${o}.${m.name}(..${paramValues}).freek[I].onionX1[O] }"
        }

        val implem =
          q"""{
          new ${o}.TypeClass[M] {
            ..${funcs.map(f => generateFreekoImpl(f))}
          }
        }"""
        typeClass.typeParams.size match {
          case 1 => Some(q"implicit def ${TermName(c.freshName("impl"))} = $implem")
          case 0 => None
        }
      }

      def mergedObjects = sortedObjects.zipWithIndex.map { case (o, i) => q"val ${TermName(s"mergedObject$i")} = $o" }

      val res = c.Expr(
        q"""
        class Context extends freedsl.dsl.MergedDSLObject { self =>
          ..$mergedObjects

          import freek._
          import cats._
          import cats.implicits._

          type I = $I
          type OL[T] = Either[freedsl.dsl.Error, T]
          type O = OL :&: Bulb

          val DSLInstance = freek.DSL.Make[I]
          $mType

          lazy val implicits = new {
            import freek._
            ..${sortedObjects.flatMap(o => implicitFunction(o))}
          }
       }
       new Context
      """)

      res
    }


    def extractInnerMergedObjects(objects: Seq[c.Expr[freedsl.dsl.MergedDSLObject]]): Seq[c.Expr[freedsl.dsl.DSLObject]] = {
      import c.universe._

      val dslObjectType = weakTypeOf[DSLObject]

      def objectValues = objects.flatMap { o =>
        o.actualType.members.filter(_.typeSignature.finalResultType <:< dslObjectType).toSeq.map { res =>
          q"$o.$res"
        }
      }

      objectValues.map(o => c.Expr[freedsl.dsl.DSLObject](o))
    }


    val mergedDSLObjectType = weakTypeOf[freedsl.dsl.MergedDSLObject]
    val (mergedDSLObjectsP, dslObjectsP) = objects.partition(_.tree.symbol.typeSignature.finalResultType <:< mergedDSLObjectType)
    val dslObjects = dslObjectsP.toSeq.asInstanceOf[Seq[c.Expr[DSLObject]]]
    val mergedDSLObjects = mergedDSLObjectsP.toSeq.asInstanceOf[Seq[c.Expr[MergedDSLObject]]]

    if(mergedDSLObjects.isEmpty) mergeDSLObjects(dslObjects)
    else {
      val innerMerged = extractInnerMergedObjects(mergedDSLObjects)
      mergeDSLObjects(dslObjects ++ innerMerged)
    }

  }

  def merge(objects: freedsl.dsl.MergeableDSLObject*) = macro context_impl


  def mergeInterpreters_impl(c: Context)(objects: c.Expr[freedsl.dsl.MergeableDSLInterpreter]*): c.Expr[freedsl.dsl.MergedDSLInterpreter] = {
    import c.universe._

    val dslObjectType = weakTypeOf[DSLInterpreter]
    val dslErrorTye = weakTypeOf[Error]

    def mergeInterpreters(objects: Seq[c.Expr[DSLInterpreter]]): c.Expr[MergedDSLInterpreter] = {
      def distinct(interpreters: List[Tree], result: List[Tree], seen: Set[String]): List[Tree] =
        interpreters match {
          case Nil => result.reverse
          case h :: t =>
            val id = extractObjectIdentifier(c)(h)
            if (seen.contains(id)) distinct(t, result, seen)
            else distinct(t, h :: result, seen + id)
        }

      val sortedObjects = distinct(objects.map(_.tree).toList, List.empty, Set.empty).sortBy(o => extractObjectIdentifier(c)(o))
      val freekInterpreter = sortedObjects.map(x => x).reduceRight((o1, o2) => q"$o1 :&: $o2": Tree)

      val stableTerms = (sortedObjects.zipWithIndex).map { case (o, i) => TermName(s"o$i") }
      val stableIdentifiers = (sortedObjects zip stableTerms).map { case (o, t) =>
        q"val $t = $o"
      }

      val companions = stableTerms.map(t => q"$t.companion")
      val mergedDSLInterpreterType = weakTypeOf[MergedDSLInterpreter]
      val interpreters = q"""List[$dslObjectType](..${sortedObjects.map(o => c.Expr[DSLInterpreter](o)).map(o => q"$o")})"""

      val res =
        q"""
      import scala.language.experimental.macros
      import freek._

      class InterpretationContext extends $mergedDSLInterpreterType {
        ..$stableIdentifiers

        val merged = freedsl.dsl.merge(..$companions)
        type M[T] = merged.M[T]

        lazy val implicits = merged.implicits

        def run[T](program: M[T]) = {
          def foldError(eithers: List[Either[$dslErrorTye, Unit]]): Either[$dslErrorTye, Unit] =
            eithers match {
              case Nil => Right(())
              case h :: t =>
                h match {
                  case Right(()) => foldError(t)
                  case Left(e) => Left(e)
                }
            }

          lazy val interpreter = $freekInterpreter
          val res = program.value.interpret(interpreter)
          val termination = foldError($interpreters.map(_.terminate))
          for {
            v <- res
            _ <- termination
          } yield v
        }
      }

      new InterpretationContext()"""

      c.Expr[MergedDSLInterpreter](res)
    }

    def extractInterpreters(mergedDSLInterpreter: c.Expr[MergedDSLInterpreter]): List[c.Expr[DSLInterpreter]] = {
      import c.universe._

      val objectValues =
        mergedDSLInterpreter.actualType.members.filter(_.typeSignature.finalResultType <:< dslObjectType).map { res => q"$mergedDSLInterpreter.$res" }

      objectValues.map(o => c.Expr[freedsl.dsl.DSLInterpreter](o)).toList
    }

    val mergedDSLInterpreterType = weakTypeOf[MergedDSLInterpreter]

    def extraction(toProcess: List[c.Expr[MergeableDSLInterpreter]], result: List[c.Expr[DSLInterpreter]]): List[c.Expr[DSLInterpreter]]  =
      toProcess match {
        case Nil => result.reverse
        case h :: t =>
          if(h.tree.symbol.typeSignature.finalResultType <:< mergedDSLInterpreterType) extraction(t, extractInterpreters(h.asInstanceOf[c.Expr[MergedDSLInterpreter]]) ::: result)
          else extraction(t, h.asInstanceOf[c.Expr[DSLInterpreter]] :: result)
      }


    mergeInterpreters(extraction(objects.toList, List.empty))

  }

  def merge(objects: freedsl.dsl.MergeableDSLInterpreter*) = macro mergeInterpreters_impl

}
