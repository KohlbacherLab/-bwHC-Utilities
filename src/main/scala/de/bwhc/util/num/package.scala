package de.bwhc.util




package object num
{

  implicit class DoubleFormattingOps(val d: Double) extends AnyVal
  {
    import scala.math.BigDecimal

    def withDecimals(n: Int): Double =
      BigDecimal(d).setScale(n, BigDecimal.RoundingMode.HALF_UP).toDouble
  }


}
