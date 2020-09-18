package de.bwhc.util.data



import java.time.temporal.Temporal


import scala.util.matching.Regex

import cats.data.{
  NonEmptyList,
  Validated,
  ValidatedNel
}
import Validated.{cond,condNel}

import cats.Traverse
import cats.syntax.traverse._



object Validation
{
  self =>


  type Validator[E,T] = T => ValidatedNel[E,T] 


  def attempt[T](t: => T): ValidatedNel[Throwable,T] =
    Validated.catchNonFatal(t).toValidatedNel


  def ifThrows[T,E](t: => T)(err: => E) =
    attempt(t) otherwise err


  implicit class ValidatedOps[E,T](val v: ValidatedNel[E,T]) extends AnyVal
  {
    def orError[EE](err: EE) =
      v.leftMap(_ => NonEmptyList.one(err))

    def otherwise[EE](err: EE) =
      v.leftMap(_ => NonEmptyList.one(err))

    def mapErrors[EE](f: E => EE) =
      v.leftMap(_.map(f))
  }



  implicit class IterableOps[T, C[X] <: Iterable[X]](val ts: C[T]) extends AnyVal
  {

    def ifEmpty[E](err: => E) = condNel(!ts.isEmpty,ts,err)

  }


  //---------------------------------------------------------------------------
  // Traversable
  //---------------------------------------------------------------------------

  implicit class TraversableOps[T, C[T]: Traverse](val ts: C[T])
  {
    def validateEach[E](implicit v: Validator[E,T]) = ts.traverse(v)   
  }


  implicit class OptionOps[T](val opt: Option[T]) extends AnyVal
  {

    def ifUndefined[E](err: => E) = condNel(opt.isDefined,opt.get,err)

  }


  implicit class ValueOps[T](val t: T) extends AnyVal
  {

    def validate[E](implicit v: Validator[E,T]): ValidatedNel[E,T] = v(t)

  }


  object dsl 
  {

    @annotation.implicitNotFound("${T} is not of type Option[X]")
    sealed trait IsOption[T]
    object IsOption
    {
      def apply[T](implicit opt: IsOption[T]) = opt

      implicit def option[T] = new IsOption[Option[T]]{} 
    }


    @annotation.implicitNotFound("${T} is not a sub-type of Iterable[X]")
    sealed trait IsIterable[T]
    object IsIterable
    {
      def apply[T](implicit is: IsIterable[T]) = is

      implicit def iterable[T, C[X] <: Iterable[X]] = new IsIterable[C[T]]{} 
    }


/*
    val defined: Validator[String,Option[Any]] =
      t => condNel(t.isDefined, t, s"$t is not defined")

    val undefined: Validator[String,Option[Any]] =
      t => condNel(t.isEmpty, t, s"$t is defined")
*/

    sealed trait Defined

    val defined = new Defined{}

    sealed trait Undefined

    val undefined = new Undefined{}

    sealed trait Empty

    val empty = new Empty{}



    def be[T](t: T) = equal(t)

    def be(b: Boolean) = 
      (t: Boolean) => condNel(t, t, s"$t not $b") 


    def be[T](v: Validator[_,T]) = v 


    def not[T](v: Validator[_,T]) = 
      (t: T) => condNel(v(t).isInvalid, t, s"$v failed for $t") 



    def matchRegex(regex: Regex) =
      (s: String) => condNel(regex matches s, s, s"$s doesn't match $regex")

    def matchRegex(regex: String): Validator[String,String] =
      matchRegex(regex.r)



    def contain[T](t: T) =
      (ts: Iterable[T]) => condNel(ts.exists(_ == t), ts, s"$t not in $ts")


    def in[T](ts: Iterable[T]) =
      (t: T) => condNel(ts.exists(_ == t), t, s"$t not in $ts")


    def in[T](interval: Interval[T]) = 
      (t: T) => condNel(interval contains t, t, s"$t not in $interval")




    def equal[T](u: T) =
      (t: T) => condNel(t == u, t, s"$t not equal to $u")


    def lessThan[T](u: T)(implicit ord: Ordering[T]) = 
      (t: T) => condNel(ord.lt(t,u), t, s"$t not less than $u")

//    def <=[T](u: T)(implicit ord: Ordering[T]) = lessThan(u)

    def before[T](u: T)(implicit ord: Ordering[T]) = lessThan(u)

    def greaterThan[T](u: T)(implicit ord: Ordering[T]) = 
      (t: T) => condNel(ord.gt(t,u), t, s"$t not greater than $u")

//    def >=[T](u: T)(implicit ord: Ordering[T]) = greaterThan(u)

    def after[T](u: T)(implicit ord: Ordering[T]) = greaterThan(u)



    case class MustBe[T](t: T) extends AnyVal
    {

      def apply(d: Defined)(implicit opt: IsOption[T]) = 
        condNel(t.asInstanceOf[Option[Any]].isDefined, t, s"$t is not defined")

      def apply(ud: Undefined)(implicit opt: IsOption[T]) = 
        condNel(!t.asInstanceOf[Option[Any]].isDefined, t, s"$t must be undefined")

      def apply(e: Empty)(implicit it: IsIterable[T]) = 
        condNel(t.asInstanceOf[Iterable[Any]].isEmpty, t, s"$t is not empty")

    }

/*
    case class Must[T](t: T) extends AnyVal
    {

      def apply(v: Validator[_,T]) = v(t)  


    }

    case class Be[T](t: T) extends AnyVal
    {

      def apply(u: T) =
        condNel(t == u, t, s"$t not equal to $u")

      def apply(v: Validator[_,T]) = v(t)

      def apply(d: Defined)(implicit opt: IsOption[T]) = 
        condNel(t.asInstanceOf[Option[Any]].isDefined, t, s"$t is not defined")

    }
*/

    implicit class OptionSyntax[T](val t: Option[T]) extends AnyVal
    {

      def mustBe   = MustBe(t)
      def shouldBe = MustBe(t)
      def couldBe = MustBe(t)

    }


    implicit class ValueSyntax[T](val t: T) extends AnyVal
    {

      def must(v: Validator[_,T]) = v(t)

      def must = MustBe(t)


      def mustNot(v: Validator[_,T]) = 
        condNel(v(t).isInvalid, t, s"$v failed for $t") 


      def should(v: Validator[_,T]) = t must v

      def shouldNot(v: Validator[_,T]) = t mustNot v

      def could(v: Validator[_,T]) = t must v

    }


    implicit class IterableSyntax[T, C[X] <: Iterable[X]](val ts: C[T]) extends AnyVal
    {

      def must(v: Validator[_,Iterable[T]]) = v(ts.asInstanceOf[Iterable[T]])

      def should(v: Validator[_,Iterable[T]]) = v(ts.asInstanceOf[Iterable[T]])

    }


  }
  


}
