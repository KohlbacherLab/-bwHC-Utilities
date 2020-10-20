package de.bwhc.util


package object mapping 
{


import cats.Monoid

import shapeless.{
  HList, HNil, ::,
  Generic, LabelledGeneric,
  Lazy,
  Poly, Poly1
}
import shapeless.labelled.{
  field, FieldType
}
import shapeless.ops.hlist


/*
  Adapted from Chapter 6 in

  D. Gurnell - The Type Astronaut's Guide to Shapeless

  https://underscore.io/training/courses/advanced-shapeless/
*/



  // Type alias needed because variance of Function[-A,+B]
  // leads to problems with implicit resolution in generic derivation

  type Mapping[A,B] = A => B



  implicit val hnilMonoid: Monoid[HNil] =
    Monoid.instance[HNil](HNil, (_,_) => HNil)

  implicit def hlistMonoid[K <: Symbol, H, T <: HList](
    implicit
    hMonoid: Lazy[Monoid[H]],
    tMonoid: Monoid[T]
  ): Monoid[FieldType[K,H] :: T] =
    Monoid.instance[FieldType[K,H] :: T](
      field[K](hMonoid.value.empty) :: tMonoid.empty,
      (x,y) => field[K](hMonoid.value.combine(x.head,y.head)) :: tMonoid.combine(x.tail,y.tail)
    )


  implicit def genericMapping[
    A, B,
    Apr <: HList,
    Bpr <: HList,
    Common <: HList,
    Added <: HList,
    Unaligned <: HList
  ](
    implicit
    aGen    : LabelledGeneric.Aux[A,Apr],
    bGen    : LabelledGeneric.Aux[B,Bpr],
    inter   : hlist.Intersection.Aux[Apr,Bpr,Common],
    diff    : hlist.Diff.Aux[Bpr,Common,Added],
    monoid  : Monoid[Added],
    prepend : hlist.Prepend.Aux[Added,Common,Unaligned],
    align   : hlist.Align[Unaligned,Bpr]
  ): Mapping[A,B] = {
    a => bGen.from(align(prepend(monoid.empty,inter(aGen.to(a)))))
  }


  def deriveMapping[A,B](implicit f: Mapping[A,B]) = f





}
