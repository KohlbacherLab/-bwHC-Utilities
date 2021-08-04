package de.bwhc.util.num


import org.scalatest.flatspec.AnyFlatSpec

import scala.math.Pi


class Tests extends AnyFlatSpec
{

  "Double rounding" must "work" in {

     assert(Pi.withDecimals(4) == 3.1416)
     

  }

}
