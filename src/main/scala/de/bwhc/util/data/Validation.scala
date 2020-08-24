package de.bwhc.util.data



import java.time.temporal.Temporal

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


    //-------------------------------------------------------------------------
    // Ordering Ops
    //-------------------------------------------------------------------------

//    def mustBe(v: Validator[String,T]) = v(t)

//    def mustBe(u: T) =
//      condNel(t == u, t, s"$t not equal to $u")

/*
    def mustBeLess(u: T)(implicit o: Ordering[T]) =
      condNel(o.lt(t,u), t, s"$t not less than $u")


    def mustBeLessOrEq(u: T)(implicit o: Ordering[T]) =
      condNel(o.lteq(t,u), t, s"$t not less than or equal to $u")


    def mustBeGreater(u: T)(implicit o: Ordering[T]) =
      condNel(o.gt(t,u), t, s"$t not greater than $u")


    def mustBeGreaterOrEq(u: T)(implicit o: Ordering[T]) =
      condNel(o.gteq(t,u), t, s"$t not greater than or equal to $u")
*/

  }


  object dsl 
  {

    sealed trait Defined

    val defined = new Defined{}


    @annotation.implicitNotFound("${T} is not of type Option[_]")
    sealed trait IsOption[T]
    object IsOption
    {
      def apply[T](implicit opt: IsOption[T]) = opt

      implicit def option[T] = new IsOption[Option[T]]{} 
    }


    def in[T](ts: Iterable[T]) =
      (t: T) => condNel(ts.find(_ == t).isDefined, t, s"$t not in Iterable collection")


    def in[T](interval: Interval[T]) = 
      (t: T) => condNel(interval contains t, t, s"$t not in $interval")


    def lessThan[T](u: T)(implicit ord: Ordering[T]) = 
      (t: T) => condNel(ord.lt(t,u), t, s"$t not less than $u")

    def <=[T](u: T)(implicit ord: Ordering[T]) = lessThan(u)

    def before[T](u: T)(implicit ord: Ordering[T]) = lessThan(u)




    case class MustBe[T](t: T) extends AnyVal
    {

      def apply(u: T) =
        condNel(t == u, t, s"$t not equal to $u")    

      
      def apply(d: Defined)(implicit opt: IsOption[T]) = 
        condNel(t.asInstanceOf[Option[Any]].isDefined, t, s"$t is not defined")

      def apply(v: Validator[_,T]) =
        v(t)

    }


    implicit class Syntax[T](val t: T) extends AnyVal
    {

      def mustBe = MustBe(t)

      def shouldBe = MustBe(t)

    }

  }
  


}
