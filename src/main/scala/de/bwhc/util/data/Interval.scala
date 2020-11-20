package de.bwhc.util.data



import play.api.libs.json.{Format,Json}


sealed abstract class Interval[T: Ordering]
{
  def contains(t: T): Boolean
  def ∋ (t: T) = contains(t)
  override def toString: String
}

import Interval._

case class ClosedInterval[T: Ordering](l: T, r: T) extends Interval[T]
{
  def contains(t: T): Boolean = (l <= t && t <= r)
  override def toString = s"[$l,$r]"
}
object ClosedInterval
{

  def apply[T: Ordering](lr: (T,T)): ClosedInterval[T] =
    ClosedInterval(lr._1,lr._2)

  implicit def format[T: Ordering: Format] =
    Json.format[ClosedInterval[T]]
}

case class LeftOpenInterval[T: Ordering](l: T, r: T) extends Interval[T]
{
  def contains(t: T): Boolean = (l < t && t <= r)
  override def toString = s"($l,$r]"
}
object LeftOpenInterval
{
  def apply[T: Ordering](lr: (T,T)): LeftOpenInterval[T] =
    LeftOpenInterval(lr._1,lr._2)

  implicit def format[T: Ordering: Format] =
    Json.format[LeftOpenInterval[T]]
}

case class RightOpenInterval[T: Ordering](l: T, r: T) extends Interval[T]
{
  def apply[T: Ordering](lr: (T,T)): RightOpenInterval[T] =
    RightOpenInterval(lr._1,lr._2)

  def contains(t: T): Boolean = (l <= t && t < r)
  override def toString = s"[$l,$r)"
}
object RightOpenInterval
{
  implicit def format[T: Ordering: Format] =
    Json.format[RightOpenInterval[T]]
}

case class OpenInterval[T: Ordering](l: T, r: T) extends Interval[T]
{
  def apply[T: Ordering](lr: (T,T)): OpenInterval[T] =
    OpenInterval(lr._1,lr._2)

  def contains(t: T): Boolean = (l < t && t < r)
  override def toString = s"($l,$r)"
}
object OpenInterval
{
  implicit def format[T: Ordering: Format] =
    Json.format[OpenInterval[T]]
}


object Interval
{

  implicit class OrderingOps[T](val t: T) extends AnyVal
  {
    def <(u: T)(implicit o: Ordering[T])  = o.lt(t,u)
    def <=(u: T)(implicit o: Ordering[T]) = o.lteq(t,u)
    def >(u: T)(implicit o: Ordering[T])  = o.gt(t,u)
    def >=(u: T)(implicit o: Ordering[T]) = o.gteq(t,u)
  }


  implicit class NumericOps[T](val t: T) extends AnyVal
  {
    def +-(m: T)(implicit num: Numeric[T]) =
      ClosedInterval(num.minus(t,m),num.plus(t,m))
  }

/*
  case class Closed[T: Ordering](l: T, r: T) extends Interval[T]
  {
    def contains(t: T): Boolean = (l <= t && t <= r)
    override def toString = s"[$l,$r]"
  }

  case class LeftOpen[T: Ordering](l: T, r: T) extends Interval[T]
  {
    def contains(t: T): Boolean = (l < t && t <= r)
    override def toString = s"($l,$r]"
  }

  case class RightOpen[T: Ordering](l: T, r: T) extends Interval[T]
  {
    def contains(t: T): Boolean = (l <= t && t < r)
    override def toString = s"[$l,$r)"
  }

  case class Open[T: Ordering](l: T, r: T) extends Interval[T]
  {
    def contains(t: T): Boolean = (l < t && t < r)
    override def toString = s"($l,$r)"
  }
*/

  implicit class IntervalOps[T](val t: T) extends AnyVal
  {
    def isIn(interval: Interval[T]) = interval contains t
    def ∈ (interval: Interval[T]) = t isIn interval
  }

/*
  implicit def formatClosed[T: Ordering: Format] =
    Json.format[Closed[T]]

  implicit def formatOpen[T: Ordering: Format] =
    Json.format[Open[T]]

  implicit def formatLeftOpen[T: Ordering: Format] =
    Json.format[LeftOpen[T]]

  implicit def formatRightOpen[T: Ordering: Format] =
    Json.format[RightOpen[T]]
*/

}


