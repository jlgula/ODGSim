package org.opendcgrid.app.sim

import scala.util.{Success, Try}

object GridBuilder {
  def build(input: String): Try[Grid] = Success(new Grid())
}
