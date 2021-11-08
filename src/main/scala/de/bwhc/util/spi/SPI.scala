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



trait ServiceProviderInterfaceF
{
  type Service[F[_]]

  def getInstance[F[_]]: Service[F]
}


trait EnvSPI[S[F[_]]] extends ServiceProviderInterfaceF
{
  type Service[M[_]] = S[M]
}


abstract class SPILoaderF[S <: ServiceProviderInterfaceF]
(
  val spi: Class[S]
)
{

  def getInstance[F[_]]: Try[S#Service[F]] =
    Try {
      ServiceLoader.load(spi)
        .iterator
        .next
        .getInstance[F]
    }

}

/*
trait EnvServiceProviderInterface
{
  type Env[F[_]]
  type Service

  def getInstance[F[_]: Env]: Service
}


trait EnvSPI[S, E[F[_]]] extends EnvServiceProviderInterface
{
  type Service = S
  type Env[F[_]] = E[F]
}


abstract class EnvSPILoader[S <: EnvServiceProviderInterface]
(
  val spi: Class[S]
)
{

//  def getInstance[F[_]](implicit env: S#Env[F]): Try[S#Service] =
  def getInstance[F[_]: S#Env]: Try[S#Service] =
    Try {
      ServiceLoader.load(spi)
        .iterator
        .next
        .getInstance[F]
    }

}
*/

