package fp

import scala.language.higherKinds
import scalaz.~>

trait CopKInjectable[IN[_], OUT[_]] {
  def inject: IN ~> OUT
  def project: OUT ~> λ[a => Option[IN[a]]]
}
