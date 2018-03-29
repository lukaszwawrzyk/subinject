package fp

import iotaz.CopK

import scala.language.experimental.macros
import scala.language.higherKinds

object SubInject {
  def summon[Inner[a] <: CopK[_, a], Outer[a] <: CopK[_, a]]: Injectable[Inner, Outer] = macro iotaz.internal.SubInjectMacros.summon[Inner, Outer]
}