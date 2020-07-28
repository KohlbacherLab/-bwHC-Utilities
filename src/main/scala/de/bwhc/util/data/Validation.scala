package de.bwhc.util.data



import cats.data.{
  NonEmptyList,
  Validated,
  ValidatedNel
}

import cats.Traverse


object Validation
{
  self =>

  import Validated.{cond,condNel}

  import cats.syntax.validated._
  import cats.implicits._


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

    def nonEmpty =
      condNel(!ts.isEmpty,ts,"Iterable empty")

    def ? = nonEmpty

    def ifEmpty[E](err: => E) = (ts ?) otherwise err

  }

  //---------------------------------------------------------------------------
  // Traversable
  //---------------------------------------------------------------------------

  implicit class TraversableOps[T, C[T]: Traverse](val ts: C[T])
  {
    def validate[E](implicit v: Validator[E,T]) = ts.traverse(v)   
  }



  implicit class OptionOps[T](val opt: Option[T]) extends AnyVal
  {

    def defined =
      condNel(opt.isDefined,opt.get,"Option undefined")

    def ? = defined

    def ifUndefined[E](err: => E) = (opt ?) otherwise err 
  }



  implicit class ValueOps[T](val t: T) extends AnyVal
  {

    def validate[E](implicit v: Validator[E,T]): ValidatedNel[E,T] = v(t)


    def mustBeIn(interval: Interval[T]) =
      condNel(interval contains t, t, s"$t not in $interval")

    def mustBeIn(ts: Iterable[T]) =
      (ts.find(_ == t) ?) otherwise s"$t not in Iterable collection"


    def ifNotIn[E](ts: Iterable[T])(err: => E) = (t mustBeIn ts) otherwise err 

    def ifNotIn[E](i: Interval[T])(err: => E) = (t mustBeIn i) otherwise err 



    //-------------------------------------------------------------------------
    // Ordering Ops
    //-------------------------------------------------------------------------

    def mustBe(u: T) =
      condNel(t == u, t, s"$t not equal to $u")


    def mustBeLess(u: T)(implicit o: Ordering[T]) =
      condNel(o.lt(t,u), t, s"$t not less than $u")


    def mustBeLessOrEq(u: T)(implicit o: Ordering[T]) =
      condNel(o.lteq(t,u), t, s"$t not less than or equal to $u")


    def mustBeGreater(u: T)(implicit o: Ordering[T]) =
      condNel(o.gt(t,u), t, s"$t not greater than $u")


    def mustBeGreaterOrEq(u: T)(implicit o: Ordering[T]) =
      condNel(o.gteq(t,u), t, s"$t not greater than or equal to $u")


  }


}
