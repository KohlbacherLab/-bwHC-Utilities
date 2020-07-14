package de.bwhc.util.spi


import java.util.ServiceLoader

import scala.util.Try


trait ServiceProviderInterface
{
  type Service

  def getInstance: Service
}

trait SPI[T] extends ServiceProviderInterface
{
  type Service = T
}


abstract class SPILoader[S <: ServiceProviderInterface]
(
  val spi: Class[S]
)
{
  def getInstance: Try[S#Service] =
    Try {
      ServiceLoader.load(spi)
        .iterator
        .next
        .getInstance
    }

}
