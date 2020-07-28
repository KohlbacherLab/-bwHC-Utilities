package de.bwhc.util.data



import cats.data.{
  NonEmptyList,
  Validated,
  ValidatedNel
}


object Validators
{
  self =>

  type Validator[T] = T => ValidatedNel[String,T] 


  import Validated.{cond,condNel}

  import cats.syntax.validated._

  import cats.implicits._


  def succeed[T]: Validator[T] =
    t => Validated.validNel[String,T](t)


  def isDefined[T]: Validator[Option[T]] =
   opt => condNel(opt.isDefined,opt,"Undefined value")


  def imap[A,B](va: Validator[A])(f: B => A, g: A => B): Validator[B] =
    b => va(f(b)).map(g)


  def product[A,B](va: Validator[A], vb: Validator[B]): Validator[(A,B)] =
    (ab: (A,B)) => (va(ab._1),vb(ab._2)).mapN((a,b) => (a,b))
  

  def or[A](v1: Validator[A], v2: => Validator[A]): Validator[A] = 
    a => v1(a).orElse(v2(a))

/*
  trait CanBuildValidator
  {
    def apply[A,B](f: B => A, g: A => B): Validator[B]
  }

  
  trait ValidatorBuilder
  {
    
  }
*/


  implicit class Syntax[A](val va: Validator[A]) extends AnyVal
  {

    def and[B](vb: Validator[B]) = self.product(va,vb)
    def ~[B](vb: Validator[B]) = self.product(va,vb)

    def or(v2: Validator[A]) = self.or(va,v2)
    def |(v2: Validator[A])  = self.or(va,v2)

    def imap[B](f: B => A, g: A => B) = self.imap(va)(f,g)

  }


}


