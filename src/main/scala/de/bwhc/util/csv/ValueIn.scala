package de.bwhc.util.csv


import shapeless.{
  Generic, DepFn1,
  HList, ::, HNil
}


@annotation.implicitNotFound("Couldn't find ValueIn[${T}] instance: define one of ensure it's in scope")
sealed abstract class ValueIn[T] extends DepFn1[T]

object ValueIn
{


  type Aux[T,V] = ValueIn[T]{ type Out = V }

  def apply[T <: AnyVal](implicit value: ValueIn[T]): Aux[T,value.Out] = value

  implicit def valueType[T <: AnyVal,Tpr,V](
    implicit
    gen: Generic.Aux[T,Tpr],
    value: ValueIn.Aux[Tpr,V]
  ): Aux[T,value.Out] =
    new ValueIn[T]{
      type Out = V
      override def apply(t: T): Out =
        value(gen.to(t))
    }

  implicit def monoHListCsvValue[H]: Aux[H :: HNil,H] =
    new ValueIn[H :: HNil]{
      type Out = H
      override def apply(h: H :: HNil): Out = h.head
    }

}
