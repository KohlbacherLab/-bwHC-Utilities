package de.bwhc.util.csv


import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor



sealed trait CsvWriter[T] extends (T => CsvValue)
{
  self =>

  val headers: CsvValue

  def apply(t: T): CsvValue = self.write(t)

  def write(t: T): CsvValue 

  def on[U](f: U => T): CsvWriter[U] =
    new CsvWriter[U]{

      val headers = self.headers

      def write(u: U) = self.write(f(u))
    }
  
}


object CsvWriter
{

  def apply[T](
    f: T => String
  ): CsvWriter[T] =
    new CsvWriter[T]{

      override val headers = CsvValue.empty

      override def write(t: T) = CsvValue(f(t))
    } 


  def of[T](
    hw: (String, T => CsvValue),
    hws: (String, T => CsvValue)*
  ): CsvWriter[T] =
    of(hw +: hws)


  private def of[T](
    hws: Seq[(String, T => CsvValue)]
  ): CsvWriter[T] = {

    val (hs,ws) = hws.toList.unzip

    new CsvWriter[T]{

      override val headers = CsvValue(hs)

      override def write(t: T) =
        ws.foldLeft[CsvValue](CsvValue.empty)((acc,f) => acc :+ f(t))
    } 

  }


  def of[T <: TemporalAccessor](f: DateTimeFormatter): CsvWriter[T] =
    CsvWriter[T](f.format)


  def on[T,U](f: U => T)(implicit csv: CsvWriter[T]): CsvWriter[U] =
    csv.on[U](f)



  sealed trait Builder[U]{
    def apply[T](f: U => T)(implicit csv: CsvWriter[T]): CsvWriter[U] =
      csv.on(f)
  } 

  def on[U] = new Builder[U]{}



  implicit val stringWriter =
    CsvWriter[String](identity) 

  implicit val intWriter =
    CsvWriter[Int](_.toString)
 
  implicit val longWriter =
    CsvWriter[Long](_.toString) 
 
  implicit val floatWriter =
    CsvWriter[Float](_.toString) 
 
  implicit val doubleWriter =
    CsvWriter[Double](_.toString) 
 
  implicit val booleanWriter =
    CsvWriter[Boolean](_.toString) 

    
  implicit def option[T](
    implicit w: CsvWriter[T]
  ): CsvWriter[Option[T]] =  
    new CsvWriter[Option[T]]{
 
      override val headers =
        w.headers
 
      override def write(opt: Option[T]) =
        opt.map(w.write).getOrElse(CsvValue.empty)        
    } 



  object temporal
  {

    import java.time.{
      LocalDate,
      LocalDateTime,
      Instant,
      YearMonth
    }
    import DateTimeFormatter.{
      ISO_LOCAL_DATE,
      ISO_LOCAL_DATE_TIME,
      ISO_INSTANT
    }


    implicit val localDateWriter =
      CsvWriter[LocalDate](ISO_LOCAL_DATE.format) 
  
    implicit val localDateTimeWriter =
      CsvWriter[LocalDateTime](ISO_LOCAL_DATE_TIME.format) 
  
    implicit val instantWriter =
      CsvWriter[Instant](ISO_INSTANT.format) 

    implicit val yearMonthWriter =
      CsvWriter[YearMonth](DateTimeFormatter.ofPattern("yyyy-MM").format) 

  }



  def of[T](implicit csv: CsvWriter[T]) = csv

  object derivation
  {


    import shapeless.{
      LabelledGeneric,
      Witness,
      HList, ::, HNil,
      Lazy
    }
    import shapeless.labelled.FieldType
 

    implicit def valueTypeWriter[T <: AnyVal,V](
      implicit
      vt: ValueIn.Aux[T,V],
      w: CsvWriter[V]
    ): CsvWriter[T] =  
      new CsvWriter[T]{
 
        override val headers =
          w.headers
 
        override def write(t: T) =
          w.write(vt(t))
      } 
 
 
    implicit def genericWriter[T,Tpr](
      implicit
      gen: LabelledGeneric.Aux[T,Tpr],
      tprWriter: CsvWriter[Tpr]
    ): CsvWriter[T] =
      new CsvWriter[T]{
 
        override val headers =
          tprWriter.headers
 
        override def write(t: T) =
          tprWriter.write(gen.to(t))
      } 
 
 
    implicit def hlistWriter[Name <: Symbol, H, T <: HList](
      implicit
      w: Witness.Aux[Name],
      hWriter: Lazy[CsvWriter[H]],
      tWriter: CsvWriter[T]
    ): CsvWriter[FieldType[Name,H] :: T] =
      new CsvWriter[FieldType[Name,H] :: T]{
 
        override val headers =
          CsvValue(w.value.name) :+ tWriter.headers
 
        override def write(hlist: FieldType[Name,H] :: T) =
          hWriter.value.write(hlist.head) :+ tWriter.write(hlist.tail)
      } 
 
 
    implicit val hnilWriter =  
      new CsvWriter[HNil]{
        override val headers = CsvValue.empty
        override def write(hnil: HNil) = CsvValue.empty
      } 

  }

}


