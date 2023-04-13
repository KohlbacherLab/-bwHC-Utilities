package de.bwhc.util.csv


import java.io.{
  Writer,
  FileWriter,
  File,
  OutputStream,
  FileOutputStream,
  OutputStreamWriter
}
import scala.collection.IterableOps
import cats.Semigroup


object Delimiter extends Enumeration
{
  val Pipe      = Value("|")
  val Tab       = Value("\t")
  val Semicolon = Value(";")
}



object Csv
{

  def writeTo[T, C[X] <: IterableOps[X,C,C[X]]](
    out: Writer,
    ts: C[T]
  )(
    implicit
    csv: CsvWriter[T],
    del: Delimiter.Value
  ): Unit = {

    out.write(s"${csv.headers.toCsvString}\n")
    
    for { t <- ts }{
      out.write(s"${csv.write(t).toCsvString}\n")
    }
    
    out.flush
  }


  def writeTo[T, C[X] <: IterableOps[X,C,C[X]]](
    out: OutputStream,
    ts: C[T]
  )(
    implicit
    csv: CsvWriter[T],
    del: Delimiter.Value
  ): Unit = 
    writeTo(new OutputStreamWriter(out),ts)


  def writeTo[T, C[X] <: IterableOps[X,C,C[X]]](
    out: File,
    ts: C[T]
  )(
    implicit
    csv: CsvWriter[T],
    del: Delimiter.Value
  ): Unit = 
    writeTo(new FileOutputStream(out),ts)


  object syntax
  {

    implicit class IterableCsvOps[T, C[X] <: IterableOps[X,C,C[X]]](
      val ts: C[T]
    )
    extends AnyVal
    {

      def toCsv(
        implicit csv: CsvWriter[T]
      ): C[CsvValue] =
        ts.map(csv)


      def writeCsvTo(out: Writer)(
        implicit
        csv: CsvWriter[T],
        del: Delimiter.Value
      ): Unit =
        Csv.writeTo(out,ts)

      def writeCsvTo(out: OutputStream)(
        implicit
        csv: CsvWriter[T],
        del: Delimiter.Value
      ): Unit =
        Csv.writeTo(out,ts)
    }

    implicit class CsvOps[T](val t: T) extends AnyVal
    {
      def toCsv(implicit csv: CsvWriter[T]) = csv.write(t)
    }


  }



  import shapeless.{
    LabelledGeneric, Poly2, Witness,
    HList, ::, HNil
  }
  import shapeless.labelled.FieldType
  import shapeless.ops.hlist.LeftFolder


  type CsvsByName = Map[String,(CsvValue,Seq[CsvValue])]

  implicit val csvsByNameSG: Semigroup[CsvsByName] =
    Semigroup.instance(
      (m1,m2) =>
        (m1.toSeq ++ m2.toSeq)
          .groupMapReduce(_._1)(_._2){ case ((h1 -> s1),(h2 -> s2)) => (h1 -> (s1 ++ s2)) }
    )

  private object Splitter extends Poly2
  {

    implicit def iterableCase[Name <: Symbol, T, C[X] <: Iterable[X]](
      implicit
      w: Witness.Aux[Name],
      csvWriter: CsvWriter[T]
    ): Case.Aux[CsvsByName,FieldType[Name,C[T]],CsvsByName] =
      at {
        (acc,ts) =>
          val csvs = ts.map(csvWriter)

          acc.updatedWith(w.value.name){
            case Some(headers -> seq) => Some(headers -> (seq :++ csvs))
            case None                 => Some(csvWriter.headers -> csvs.toSeq)
          }
      }


    implicit def anyCase[Name <: Symbol,T](
      implicit
      w: Witness.Aux[Name],
      csvWriter: CsvWriter[T]
    ): Case.Aux[CsvsByName,FieldType[Name,T],CsvsByName] =
      at {
        (acc,t) =>
          val csv = csvWriter(t)
          acc.updatedWith(w.value.name){
            case Some(headers -> seq) => Some(headers -> (seq :+ csv))
            case None                 => Some(csvWriter.headers -> Seq(csv))
          }
      }


    implicit def iterableSplitableCase[
      Name <: Symbol, T <: Product, C[X] <: Iterable[X], Tpr <: HList
    ](
      implicit
      w: Witness.Aux[Name],
      gen: LabelledGeneric.Aux[T,Tpr],
      lf: LeftFolder.Aux[Tpr,CsvsByName,this.type,CsvsByName]
    ): Case.Aux[CsvsByName,FieldType[Name,C[T]],CsvsByName] =
      at {
        (acc,ts) =>
          csvsByNameSG.combine(
            acc,
            ts.map(
              t =>
                gen.to(t).foldLeft(Map.empty[String,(CsvValue,Seq[CsvValue])])(this)
                  .map { case (k,csvs) => (s"${w.value.name}_$k" -> csvs)}
            )
            .reduce(csvsByNameSG.combine)
          )
      }


    implicit def anySplitableCase[
      Name <: Symbol, T <: Product, Tpr <: HList
    ](
      implicit
      w: Witness.Aux[Name],
      gen: LabelledGeneric.Aux[T,Tpr],
      lf: LeftFolder.Aux[Tpr,CsvsByName,this.type,CsvsByName]
    ): Case.Aux[CsvsByName,FieldType[Name,T],CsvsByName] =
      at {
        (acc,t) =>
          csvsByNameSG.combine(
            acc,
            gen.to(t).foldLeft(Map.empty[String,(CsvValue,Seq[CsvValue])])(this)
              .map { case (k,csvs) => (s"${w.value.name}_$k" -> csvs)}
          )
      }

  }


  def splitToCsv[T <: Product, Tpr <: HList](
    t: T
  )(
    implicit
    gen: LabelledGeneric.Aux[T,Tpr],
    lf: LeftFolder.Aux[Tpr,CsvsByName,Splitter.type,CsvsByName]
  ): lf.Out = {
    import shapeless.syntax._

    gen.to(t).foldLeft(Map.empty[String,(CsvValue,Seq[CsvValue])])(Splitter)
  }



  private final class OutWriter(
    val outdir: File
  )(
    implicit
    del: Delimiter.Value
  )
  extends (CsvsByName => Unit)
  {
    import scala.collection.mutable.Map

    private val writers = Map.empty[String,Writer]

    override def apply(csvsByName: CsvsByName) = {

      for { (name,(headers,csvs)) <- csvsByName }{

        val writer =
          writers.getOrElseUpdate(name, {
            val w = new FileWriter(new File(outdir, s"$name.csv"))
            w.write(s"${headers.toCsvString}\n")
            w
          })
        
        csvs.foreach(
          csv => writer.write(s"${csv.toCsvString}\n")
        )

        writer.flush
      }

    }

  }



  def writeTo(
    csvs: CsvsByName,
    dir: File
  )(
    implicit
    del: Delimiter.Value
  ): Unit = {

    dir.mkdirs

    new OutWriter(dir).apply(csvs)
  }


  def splitWriteToCsv[T <: Product, Tpr <: HList](
    ts: Iterable[T],
    dir: File
  )(
    implicit
    gen: LabelledGeneric.Aux[T,Tpr],
    lf: LeftFolder.Aux[Tpr,CsvsByName,Splitter.type,CsvsByName],
    del: Delimiter.Value
  ): Unit = {
    import shapeless.syntax._
    import scala.util.chaining._

    val w = new OutWriter(dir)

    ts.view
      .map(splitToCsv(_))
      .foreach(w(_))
  }

}

