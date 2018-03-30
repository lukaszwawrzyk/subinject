package test

import fp.{ CopKInjectable, SubInject }

import scalaz.~>
import iotaz.TListK.:::
import iotaz._
import iotaz.syntax.all._

import scala.language.higherKinds

object Reorder extends App {

  // todo compare actual types when matching as StrMap is now != Map[String, ?]
  type Outer[A] = CopK[List ::: Option ::: Seq ::: Map[String, ?] ::: TNilK, A]
  type Inner[A] = CopK[List ::: Map[String, ?] ::: TNilK, A]
  type StrMap[A] = Map[String, A]


  val sublistInject: CopKInjectable[Inner, Outer] = SubInject.summon[Inner, Outer]

  val sublistInject2: CopKInjectable[Inner, Outer] = new CopKInjectable[Inner, Outer] {
    override def inject: Inner ~> Outer = new ~>[Inner, Outer] {
      override def apply[A](fa: Inner[A]): Outer[A] = {
        fa.index match {
          case 0 => CopK.unsafeApply(0, fa.value)
          case 1 => CopK.unsafeApply(3, fa.value)
        }
      }
    }
    override def project: Outer ~> λ[a => Option[Inner[a]]] = new ~>[Outer, λ[a => Option[Inner[a]]]] {
      override def apply[A](fa: Outer[A]): Option[Inner[A]] = {
        fa.index match {
          case 0 => Some(CopK.unsafeApply(0, fa.value))
          case 1 => None
          case 2 => None
          case 3 => Some(CopK.unsafeApply(1, fa.value))
        }
      }
    }
  }

  val list: Inner[Int] = List(1, 2, 3).injectK[Inner]
  val map: Inner[Int] = CopK.Inject[StrMap, Inner].apply(Map("k" -> 1))

  val injectedList: Outer[Int] = sublistInject.inject(list)
  val injectedMap: Outer[Int] = sublistInject.inject(map)
  val injectedSeq: Outer[Int] = Seq(12).injectK[Outer]

  val mapOuter  = CopK.Inject[StrMap, Outer]
  val listOuter = CopK.Inject[List, Outer]

  val mapInner  = CopK.Inject[StrMap, Inner]
  val listInner = CopK.Inject[List, Inner]

  println(mapInner.prj(sublistInject.project.apply(injectedMap).get).get) // map
  println(listInner.prj(sublistInject.project.apply(injectedMap).get)) // None
  println(mapInner.prj(sublistInject.project.apply(injectedList).get)) // None
  println(sublistInject.project.apply(injectedSeq)) // none

  println(mapOuter.prj(injectedMap)) // map
  println(mapOuter.prj(injectedList)) // none

  println(listOuter.prj(injectedMap)) // none
  println(listOuter.prj(injectedList)) // list

}
