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

import java.util.{Base64, UUID}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.{Context => MacroContext}

package object dsl extends
  cats.instances.AllInstances {

  /* --------- Error and context managment ---------- */

  trait Error
  case class DSLError(cause: Throwable) extends Error
  

  object Context {
    def instance = new Context {
      def success[T](t: T): Either[Error, T] = Right(t)
      def failure[T](error: freedsl.dsl.Error): Either[Error, T] = Left(error)
    }

    def wrapError(c: Context, f: Error => Error) = new Context {
      import cats.syntax.all._
      def success[T](t: T) = c.success(t)
      def failure[T](error: freedsl.dsl.Error) = c.failure(error).leftMap(f)
    }
  }

  trait Context {
    def success[T](t: T): Either[Error, T]
    def failure[T](error: freedsl.dsl.Error): Either[Error, T]
  }

  def success[T](t: T)(implicit context: Context) = context.success(t)
  def failure[T](error: freedsl.dsl.Error)(implicit context: Context) = context.failure(error)

  trait ErrorWrapping[M[_]] {
    def wrap[T](f: Error => Error)(op: M[T]): M[T]
  }

  def wrapError[M[_]: ErrorWrapping, T](f: Error => Error)(op: M[T]) =
    implicitly[ErrorWrapping[M]].wrap(f)(op)

  implicit def result[T](t: => T)(implicit context: Context) =
    util.Try(t) match {
      case util.Success(v) => success(v)
      case util.Failure(v) => failure(DSLError(v))
    }

  def result[T](either: Either[Error, T])(implicit context: Context) =
    either match {
      case Right(v) => success(v)
      case Left(v) => failure(v)
    }

  /* ---------------- Annotations ----------------- */

  class dsl extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro dsl_impl
  }

  /* ------------------- DSL types ----------------- */

  trait DSLObjectIdentifier

  sealed trait MergeableDSLObject
  trait DSLObject extends MergeableDSLObject {
    type I[_]
    type O[_]
  }

  trait MergedDSLObject extends MergeableDSLObject

  sealed trait MergeableDSLInterpreter
  trait DSLInterpreter extends MergeableDSLInterpreter {
    def terminate(implicit context: Context) : Either[Error, Unit] = Right(())
  }
  trait MergedDSLInterpreter extends MergeableDSLInterpreter


  /* ----------- Internal dsl methods --------------- */

  case class UniqueId(id: String) extends StaticAnnotation

  private def methodSymbolId(c: MacroContext)(m: c.universe.MethodSymbol) = {
    import c.universe._

    val uniqueId =
        m.annotations.collect { case a if a.tree.tpe <:< weakTypeOf[UniqueId] =>
          val code = c.parse(a.tree.toString())
          c.eval(c.Expr[UniqueId](code)).id
        }.head

    uniqueId
  }

  private def methodDefId(c: MacroContext)(m: c.universe.DefDef) = {
    import c.universe._

    val uniqueId =
        m.mods.annotations.collect { case a if c.typecheck(a).tpe <:< weakTypeOf[UniqueId] =>
          val code = c.parse(a.toString())
          c.eval(c.Expr[UniqueId](code)).id
        }.head

    uniqueId
  }


  def abstractMethod(c: MacroContext)(m: c.universe.DefDef) = {
    import c.universe._
    !m.mods.hasFlag(Flag.PRIVATE) &&
      !m.mods.hasFlag(Flag.PROTECTED) &&
      m.mods.hasFlag(Flag.DEFERRED)
  }

  private def modifyClazz(c: MacroContext)(typeClazz: c.Tree) = {
    import c.universe._
    val q"$cmods trait $ctpname[..$ctparams] extends { ..$cearlydefns } with ..$cparents { $cself => ..$cstats }" = typeClazz

    def modifyMethod(m: DefDef, uniqueName: String) = {
      val q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" = m

      val id = c.parse(s"""new freedsl.dsl.UniqueId("$uniqueName")""")
      val newMods = m.mods.mapAnnotations(f => id :: f)
      q"""$newMods def $tname[..$tparams](...$paramss): $tpt = $expr"""
    }

    def noMethods = cstats.filter {
      case m: DefDef => !abstractMethod(c)(m)
      case _ => true
    }

    def methodUniqueName(m: DefDef, i: Int) = s"${m.name}_$i"
    val methodsWithImplicit = cstats.collect { case m: DefDef if abstractMethod(c)(m) => m }.zipWithIndex.map { case(m, i) =>
      val uniqueName = methodUniqueName(m, i)
      modifyMethod(m, uniqueName)
    }

    val modifiedClazz =
      q"""$cmods trait $ctpname[..$ctparams] extends { ..$cearlydefns } with ..$cparents { $cself =>
            ..$noMethods
            ..$methodsWithImplicit
        }"""

    modifiedClazz
  }

  def dsl_impl(c: MacroContext)(annottees: c.Expr[Any]*) = {
    import c.universe._

    def generateCompanion(typeClazz: ClassDef, comp: Tree, containerType: TypeDef) = {

      def modifyCompanion(clazz: ClassDef) = {
        val dslObjectType = weakTypeOf[DSLObject]
        val dslErrorType = weakTypeOf[freedsl.dsl.Error]
        val dslInterpreterType = weakTypeOf[freedsl.dsl.DSLInterpreter]
        val dslObjectIdentifierType = weakTypeOf[freedsl.dsl.DSLObjectIdentifier]

        val instructionName = TypeName(c.freshName("Instruction"))

        val q"$mods object $name extends ..$bases { ..$body }" = comp
        val q"$cmods trait $ctpname[..$ctparams] extends { ..$cearlydefns } with ..$cparents { $cself => ..$cstats }" = clazz

        def collect(t: cstats.type) =
          t.collect { case m: DefDef if abstractMethod(c)(m) => m }

        val abstractMethods = collect(cstats)

        val caseClassesNames = abstractMethods.map {
          m =>
            def caseClassName(func: DefDef) = methodDefId(c)(func)
            m -> caseClassName(m)
        }.toMap

        val contextName = TermName(c.freshName("context"))

        def caseClassArguments(func: DefDef) = func.vparamss.flatMap(_.map(p => q"${p.name.toTermName}: ${p.tpt}")) ++ Seq(q"$contextName: freedsl.dsl.Context")

        def generateCaseClass(func: DefDef) = {
          val params = caseClassArguments(func)
          q"case class ${TypeName(caseClassesNames(func))}[..${func.tparams}](..${params}) extends ${instructionName}[Either[$dslErrorType, ${func.tpt.children.drop(1).head}]]"
        }

        def addImplicitContext(m: c.universe.DefDef) = {
          import c.universe._
          val q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" = m
          def implicitDSL = q"implicit val ${TermName(c.freshName("context"))}: freedsl.dsl.Context"
          def addImplicitParameterList = paramss ++ List(List(implicitDSL))

          val newParamss = m.vparamss.lastOption match {
            case Some(l) if !l.isEmpty =>
              l.head match  {
                case v: ValDef if v.mods.hasFlag(Flag.IMPLICIT) =>
                  paramss.dropRight(1) ++ List(l ++ List(implicitDSL))
                case _ => addImplicitParameterList
              }
            case None => addImplicitParameterList
          }
          q"""$mods def $tname[..$tparams](...$newParamss): $tpt = $expr"""
        }

        def generateInterpreterMethod(func: DefDef) = {
          val q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" = addImplicitContext(func)
          val extractedRet = tq"${func.tpt.children.drop(1).head}"
          q"$mods def $tname[..$tparams](...$paramss): Either[$dslErrorType, $extractedRet] = $expr"
        }

        def generateMapping(func: DefDef): CaseDef = {
          val params = caseClassArguments(func).collect { case x: TermTree => val q"$n: $_" = x; TermName(n.toString) }

          def args(all: List[List[ValDef]], acc: List[List[TermName]] = Nil, offset: Int = 0): List[List[TermName]] =
            all match {
              case Nil => acc.reverse
              case h :: t =>
                args(t, params.slice(offset, offset + h.size) :: acc, offset + h.size)
            }

          val newVParamss = args(func.vparamss)
          def addContextParameterList = newVParamss ++ List(List(contextName))

          val newParamssWithContext = func.vparamss.lastOption match {
            case Some(l) if !l.isEmpty =>
              l.head match  {
                case v: ValDef if v.mods.hasFlag(Flag.IMPLICIT) =>
                  newVParamss.dropRight(1) ++ List(newVParamss.last ++ List(contextName))
                case _ => addContextParameterList
              }
            case None => addContextParameterList
          }

          cq"comp.${TermName(caseClassesNames(func))}(..${params.map(p => pq"$p")}) => ${func.name}(...${newParamssWithContext})"
        }

        val interpreterAbstractMethods = abstractMethods.map(m => generateInterpreterMethod(m))
        val interpreterMapping = abstractMethods.map(m => generateMapping(m))

        val caseClasses = abstractMethods.map(c => generateCaseClass(c))
        val objectIdentifier = UUID.randomUUID().toString
        val interpreterInputType = TypeName(c.freshName("I"))

        val packageName = {
          val typeChecked = c.typecheck(q"type Test").symbol.owner.fullName
          if(typeChecked.endsWith(".package")) typeChecked.dropRight(".package".length) else typeChecked
        }

        val typeClassName = tq"""${c.parse(packageName)}.${clazz.name}"""

        q"""
          $mods object $name extends ..$bases with $dslObjectType { comp =>
             import cats._

             def apply[M[_]](implicit m: TypeClass[M]) = m

             type ${TypeName(objectIdentifier)} = $dslObjectIdentifierType

             type TypeClass[..${clazz.tparams}] = $typeClassName[..${clazz.tparams.map(_.name)}]

             sealed trait ${instructionName}[T]
             ..${caseClasses}

             type Error = $dslErrorType

             type I[T] = ${instructionName}[T]
             type O[T] = Either[$dslErrorType, T]
             type $interpreterInputType[T] = I[T]

             trait Interpreter extends cats.~>[$interpreterInputType, cats.Id] with $dslInterpreterType {
               val companion: $name.type = comp
               type ${TypeName(objectIdentifier)} = $dslObjectIdentifierType

               ..$interpreterAbstractMethods

               def apply[A](a: $interpreterInputType[A]) = a match {
                 case ..${interpreterMapping}
               }
             }

             ..$body
          }
        """
      }

      val modifiedClazz = modifyClazz(c)(typeClazz)
      val modifiedCompanion = modifyCompanion(modifiedClazz)

      val res = c.Expr(q"""
        $modifiedClazz
        $modifiedCompanion""")

      res
    }

    def applicationConditionError =
       c.abort(
         c.enclosingPosition,
         "@dsl can only be applied to traits or abstract classes that take 1 type parameter which is a type constructor with 1 parameter"
       )

    def getContainerType(classDef: ClassDef): TypeDef =
      classDef.tparams match {
        case List(p) =>
          p.tparams match {
            case List(pp) => p
            case _ => applicationConditionError
          }
        case _ => applicationConditionError
      }

    annottees.map(_.tree) match {
      case (typeClass: ClassDef) :: Nil =>
        val t = getContainerType(typeClass)
        generateCompanion(typeClass, q"object ${typeClass.name.toTermName} {}", t)
      case (typeClass: ClassDef) :: (companion: ModuleDef) :: Nil =>
        val t = getContainerType(typeClass)
        generateCompanion(typeClass, companion, t)
      case other :: Nil => applicationConditionError
    }

  }

  def extractObjectIdentifier(c: MacroContext)(t: c.universe.Tree) = {
    val dslObjectIndentifierType = c.universe.weakTypeOf[freedsl.dsl.DSLObjectIdentifier]
    t.symbol.typeSignature.finalResultType.members.find(_.typeSignature.finalResultType <:< dslObjectIndentifierType).get.name.toString
  }


  def context_impl(c: MacroContext)(objects: c.Expr[freedsl.dsl.MergeableDSLObject]*): c.Expr[freedsl.dsl.MergedDSLObject] = {
    import c.universe._


    def mergeDSLObjects(objects: Seq[c.Expr[freedsl.dsl.DSLObject]]): c.Expr[freedsl.dsl.MergedDSLObject] = {
      val dslErrorType = weakTypeOf[freedsl.dsl.Error]
      val uniqObjects = objects.map(o => extractObjectIdentifier(c)(o.tree) -> o).toMap.values.toSeq
      val sortedObjects = uniqObjects.sortBy(o => extractObjectIdentifier(c)(o.tree))

      val I = sortedObjects.map(o => tq"${o}.I").foldRight(tq"freek.NilDSL": Tree)((o1, o2) => tq"freek.:|:[$o1, $o2]": Tree)

      val contextType = weakTypeOf[Context]
      val onionTypeName = TypeName("ONION")

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

          def caseClassName = TermName(methodSymbolId(c)(m))
          val contextName = TermName(c.freshName("context"))

          q"""def ${m.name}[..${typeParams}](...${paramss}): M[..${returns.tpe.typeArgs}] =
             cats.data.StateT { $contextName: $contextType =>
               (${o}.${caseClassName}(..${paramValues}, $contextName).freek[I].onionX1[O]: $onionTypeName[..${returns.tpe.typeArgs}]).map(a => ($contextName, a))
             }
            """
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

      val mergedObjects = sortedObjects.zipWithIndex.map { case (o, i) => q"val ${TermName(s"mergedObject$i")} = $o" }
      val caseClassMapping = sortedObjects.flatMap(o => implicitFunction(o))

      val errorType = weakTypeOf[freedsl.dsl.Error]
      val errorWrapping = q"""
        implicit def errorWraping = new freedsl.dsl.ErrorWrapping[M] {
          def wrap[T](f: $errorType => $errorType)(op: M[T]) = for {
            context <- cats.data.StateT.get[$onionTypeName, freedsl.dsl.Context]
            _ <- cats.data.StateT.set[$onionTypeName, freedsl.dsl.Context](freedsl.dsl.Context.wrapError(context, f))
            result <- op
            _ <- cats.data.StateT.set[$onionTypeName, freedsl.dsl.Context](context)
          } yield result
        }
        """

      val res = c.Expr(
        q"""
        class MergedDSL extends freedsl.dsl.MergedDSLObject { self =>
          ..$mergedObjects

          import freek._
          import cats._
          import cats.implicits._

          type I = $I
          type OL[T] = Either[$dslErrorType, T]
          type O = OL :&: Bulb

          val DSLInstance = freek.DSL.Make[I]
          type $onionTypeName[T] = freek.OnionT[cats.free.Free, DSLInstance.Cop, O, T]
          type M[T] = cats.data.StateT[$onionTypeName, $contextType, T]

          lazy val implicits = new {
            import freek._
            ..$caseClassMapping
            $errorWrapping
            implicit def ${TermName(c.freshName("context"))}: $contextType = freedsl.dsl.Context.instance
          }
       }
       new MergedDSL
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

    val res =
      if(mergedDSLObjects.isEmpty) mergeDSLObjects(dslObjects)
      else {
        val innerMerged = extractInnerMergedObjects(mergedDSLObjects)
        mergeDSLObjects(dslObjects ++ innerMerged)
      }

    res
  }

  def merge(objects: freedsl.dsl.MergeableDSLObject*) = macro context_impl

  def mergeInterpreters_impl(c: MacroContext)(objects: c.Expr[freedsl.dsl.MergeableDSLInterpreter]*): c.Expr[freedsl.dsl.MergedDSLInterpreter] = {
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
      val stableTerms = (sortedObjects.zipWithIndex).map { case (o, i) => TermName(s"o$i") }
      val stableIdentifiers = (sortedObjects zip stableTerms).map { case (o, stable) =>
        q"val $stable = $o"
      }

      val freekInterpreter = stableTerms.map(x => q"$x": Tree).reduceRight((o1, o2) => q"$o1 :&: $o2")
      val companions = stableTerms.map(t => q"$t.companion")
      val mergedDSLInterpreterType = weakTypeOf[MergedDSLInterpreter]

      val interpreters = q"""List[$dslObjectType](..${stableTerms.map(o => q"$o")})"""

      val contextName = TermName(c.freshName("context"))
      val contextType = weakTypeOf[Context]

      val res = c.Expr[MergedDSLInterpreter](
        q"""
      import scala.language.experimental.macros
      import freek._

      class InterpretationContext extends $mergedDSLInterpreterType {
        ..$stableIdentifiers

        val merged = freedsl.dsl.merge(..$companions)
        type M[T] = merged.M[T]

        lazy val implicits = merged.implicits

        def run[T](program: M[T])(implicit $contextName: $contextType) = {
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
          val res = program.runA($contextName).value.interpret(interpreter)

          val termination = foldError($interpreters.map(_.terminate($contextName)))
          for {
            v <- res
            _ <- termination
          } yield v
        }
      }

      new InterpretationContext()""")

      res
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


    val res = mergeInterpreters(extraction(objects.toList, List.empty))
    res
  }

  def merge(objects: freedsl.dsl.MergeableDSLInterpreter*) = macro mergeInterpreters_impl


}
