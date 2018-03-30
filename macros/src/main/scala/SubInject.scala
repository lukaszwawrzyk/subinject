package fp

import iotaz.CopK

import scala.language.experimental.macros
import scala.language.higherKinds

object SubInject {
  def summon[Inner[a] <: CopK[_, a], Outer[a] <: CopK[_, a]]: CopKInjectable[Inner, Outer] = macro iotaz.internal.CopKSubInjectMacros.summon[Inner, Outer]
}