package de.bwhc.util.data



import cats.data.{
  NonEmptyList,
  Validated,
  ValidatedNel
}
import Validated.{cond,condNel}

import cats.Traverse
import cats.syntax.traverse._
//import cats.syntax.validated._



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
    def validateEach[E](implicit v: Validator[E,T]) = ts.traverse(v)   
  }



  implicit class OptionOps[T](val opt: Option[T]) extends AnyVal
  {

    def mustBeDefined =
      condNel(opt.isDefined,opt.get,"Option must be defined")

    def mustNotBeDefined =
      condNel(!opt.isDefined,opt.get,"Option must not be defined")

//    def defined =
//      condNel(opt.isDefined,opt.get,"Option undefined")

//    def ? = defined

    def ? = condNel(opt.isDefined,opt.get,"Option undefined")

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

    def mustBe(v: Validator[String,T]) = v(t)

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


  object matchers
  {

    def be[T](v: Validator[String,T]) = v

//    def defined[T]: Validator[String,Option[T]] =
//      opt => condNel(opt.isDefined,opt,"Option undefined")
    def defined[T] =
      (opt: Option[T]) => condNel(opt.isDefined,opt,"Option undefined")

    def in[T](ts: Iterable[T]) =
      (t: T) => (ts.find(_ == t) ?) otherwise s"$t not in Iterable collection"

    def in[T](interval: Interval[T]) = 
      (t: T) => condNel(interval contains t, t, s"$t not in $interval")

  }



/*
  case class ValidatorBuilder[T](t: T) extends AnyVal
  {
//    def be = BeValidator(t)
  }


    case class EqualityValidator[T](t: T) extends AnyVal
    {

      def apply(u: T) =       
        condNel(t == u, t, s"$t not equal to $u")

    }


    case class BeValidator[T](t: T) extends AnyVal
    {

      def less(u: T)(implicit o: Ordering[T]) =
        condNel(o.lt(t,u), t, s"$t not less than $u")
 
 
      def lessOrEq(u: T)(implicit o: Ordering[T]) =
        condNel(o.lteq(t,u), t, s"$t not less than or equal to $u")
 
 
      def greater(u: T)(implicit o: Ordering[T]) =
        condNel(o.gt(t,u), t, s"$t not greater than $u")
 
 
      def greaterOrEq(u: T)(implicit o: Ordering[T]) =
        condNel(o.gteq(t,u), t, s"$t not greater than or equal to $u")


      def in(interval: Interval[T]) =
        condNel(interval contains t, t, s"$t not in $interval")
      

      def in(ts: Iterable[T]) =
        (ts.find(_ == t) ?) otherwise s"$t not in Iterable collection"


    }

*/


}
