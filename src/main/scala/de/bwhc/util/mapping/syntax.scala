package de.bwhc.util.mapping


//import cats.{Applicative,Monad}

 
import scala.collection.Factory


object syntax
{

  implicit class MappingOps[A](val a: A) extends AnyVal
  {

    def mapTo[B](implicit m: A => B): B = m(a)
/*
    def mapTo[F[_],B](implicit app: Applicative[F], m: A => B): F[B] =
      app.map(app.pure(a))(m)

    def mapTo[F[_],B](implicit app: Applicative[F], m: F[A => B]): F[B] =
      app.ap(m)(app.pure(a))

    def mapTo[F[_],B](implicit app: Monad[F], m: A => F[B]): F[B] =
      app.flatMap(app.pure(a))(m)
*/
  }


  implicit class IterableMappingOps[C[X] <: Iterable[X], A](val as: C[A]) extends AnyVal
  {

    def mapAllTo[B](implicit m: A => B, fac: Factory[B,C[B]]): C[B] =
      as.map(m).to(fac)

  }

}
