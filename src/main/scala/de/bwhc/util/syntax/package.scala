package de.bwhc.util

package object syntax
{

  object piping 
  {
    
    implicit class PipingOps[T](val t: T) extends AnyVal
    {
 
      def |[U](f: T => U) = f(t)

      def tee[U](f: T => U) = { f(t); t }

    } 

  }


  import shapeless.{
    HList, Generic, LabelledGeneric
  }

  object hlists
  {

    implicit class ProductToHListOps[T <: Product](val t: T) extends AnyVal
    {

      def toHList[Tpr <: HList](
        implicit gen: Generic.Aux[T,Tpr]
      ): Tpr = gen.to(t)

      def toRecord[Tpr <: HList](
        implicit gen: LabelledGeneric.Aux[T,Tpr]
      ): Tpr = gen.to(t)

    }

  }


}
