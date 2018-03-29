package iotaz.internal

import fp.Injectable
import iotaz.CopK

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.whitebox
import scalaz.Scalaz._
import scalaz._

class SubInjectMacros(val c: whitebox.Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def summon[Inner[a] <: CopK[_, a], Outer[a] <: CopK[_, a]](
    implicit
    evInner: c.WeakTypeTag[Inner[_]],
    evOuter: c.WeakTypeTag[Outer[_]]
  ): c.Expr[Injectable[Inner, Outer]] = {
    val Inner = evInner.tpe
    val Outer = evOuter.tpe

    val res = tb.foldAbort(for {
      _ <- guardAssumptions("Inner", Inner)
      _ <- guardAssumptions("Outer", Outer)
      innerTypes <- extractTypes(Inner)
      outerTypes <- extractTypes(Outer)
      innerTypeToOuterIndex <- innerTypes.traverseU { tpe =>
        findIndex(outerTypes, tpe).map(tpe -> _).toSuccessNel(s"$tpe could not be found in $Outer")
      }.map(_.toMap).toEither
    } yield {
      makeInjectable(Inner, Outer, innerTypes, outerTypes, innerTypeToOuterIndex)
    })
    res
  }

  private def makeInjectable(
    InnerT: Type,
    OuterT: Type,
    innerTypes: List[Type],
    outerTypes: List[Type],
    innerTypeToOuterIndex: Map[Type, Int]
  ): Tree = {
    val Inner = toTypeTree(InnerT)
    val Outer = toTypeTree(OuterT)
    val Transf = tq"_root_.scalaz.NaturalTransformation"
    val A = TypeName(c.freshName("A"))
    val fa = TermName(c.freshName("fa"))
    val CopK = q"_root_.iotaz.CopK"
    val projectReturnType = {
      val Lambda = TypeName(c.freshName("Lambda"))
      val a = TypeName(c.freshName("a"))
      tq"({ type $Lambda[$a] = scala.Option[$Inner[$a]] })#$Lambda"
    }

    val injectCases = innerTypes.zipWithIndex.map {
      case (tpe, index) =>
        val mappedIndex = innerTypeToOuterIndex(tpe)
        cq"$index => $CopK.unsafeApply($mappedIndex, $fa.value)"
    }

    val projectCases = {
      val projectableCases = innerTypes.zipWithIndex.map {
        case (tpe, index) =>
          val mappedIndex = innerTypeToOuterIndex(tpe)
          cq"$mappedIndex => scala.Some($CopK.unsafeApply($index, fa.value))"
      }
      val nonProjectableCases = (outerTypes.indices.toSet -- innerTypeToOuterIndex.values).map { index =>
        cq"$index => scala.None"
      }

      projectableCases ++ nonProjectableCases
    }

    q"""
       new Injectable[$Inner, $Outer] {
         override def inject: $Transf[$Inner, $Outer] = new $Transf[$Inner, $Outer] {
           override def apply[$A]($fa: $Inner[$A]): $Outer[$A] = {
             $fa.index match {
               case ..$injectCases
               case other => throw new _root_.java.lang.Exception(
                 s"subinject internal error: index " + other + " out of bounds for " + $fa)
             }
           }
         }

         override def project: $Transf[$Outer, $projectReturnType] = new $Transf[$Outer, $projectReturnType] {
           override def apply[$A](fa: $Outer[$A]): scala.Option[$Inner[$A]] = {
             $fa.index match {
               case ..$projectCases
               case other => throw new _root_.java.lang.Exception(
                 s"subinject internal error: index " + other + " out of bounds for " + $fa)
             }
           }
         }
       }
     """
  }

  private def findIndex(outerTypes: List[Type], tpe: Type) = {
    Option(outerTypes.indexOf(tpe)).filter(_ != -1)
  }

  private def extractTypes(T: Type): Either[NonEmptyList[String], List[Type]] = {
    for {
      copK <- tb.destructCopK(T).leftMap(NonEmptyList(_))
      tpes <- tb.memoizedTListKTypes(copK.L).leftMap(NonEmptyList(_))
    } yield tpes
  }

  private def guardAssumptions(
    name: String, T: Type
  ): Either[NonEmptyList[String], _] = T.resultType match {
    case _: ExistentialType => Left(NonEmptyList(
      s"type parameter $name was inferred to be existential type $T and must be specified"))
    case _ if T =:= typeOf[Nothing] => Left(NonEmptyList(
      s"type parameter $name was inferred to be Nothing and must be specified"))
    case _ => Right(())
  }

  /** Converts a `Type` to a `Tree` so that it can be safely
    * lifted into quasiquotes
    */
  private[this] final def toTypeTree(tpe: Type): Tree = tpe match {
    case poly: PolyType       => projectPoly(poly)
    case TypeRef(_, sym, Nil) => c.internal.gen.mkAttributedIdent(sym)
    case _                    => c.internal.gen.mkAttributedIdent(tpe.typeSymbol)
  }

  /** Converts an eta expanded `PolyType` such as `[z]Either[String, z]`
    * into a type lambda `Tree` `({ type ξ$[z] = Either[String, z] })#ξ$`.
    * The parameter `z` is taken from the original type and used in
    * resulting tree.
    */
  private[this] final def projectPoly(tpe: PolyType, lambdaName: TypeName = TypeName("ξ$")): Tree =
    SelectFromTypeTree(CompoundTypeTree(
      Template(
        q"_root_.scala.AnyRef" :: Nil,
        ValDef(NoMods, termNames.WILDCARD, TypeTree(), EmptyTree),
        TypeDef(NoMods, lambdaName, tpe.typeParams.map(internal.typeDef(_)),
          q"${tpe.resultType}") :: Nil)),
      lambdaName)


}
