package fp

import scala.language.higherKinds
import scalaz.~>

trait Injectable[IN[_], OUT[_]] {
  def inject: IN ~> OUT
  def project: OUT ~> λ[a => Option[IN[a]]]
}
