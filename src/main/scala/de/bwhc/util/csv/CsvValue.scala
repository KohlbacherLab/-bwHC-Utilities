package de.bwhc.util.csv


sealed trait CsvValue extends Any
{
  protected [csv] def :+(v: CsvValue): CsvValue

  def toCsvString(implicit del: Delimiter.Value): String
}


object CsvValue
{

  private [csv] case class Wrapper(
    vs: List[String]
  )
  extends AnyVal
  with CsvValue
  {
    override def :+(v: CsvValue): CsvValue =
      v match {
        case Wrapper(vprs) => Wrapper(vs :+ vprs.headOption.getOrElse(""))
      }
 
    override def toCsvString(implicit del: Delimiter.Value) =
      vs.mkString(del.toString)
  }

  val empty: CsvValue =
    Wrapper(List.empty)

  def apply(v: String): CsvValue =
    Wrapper(List(v))

  def apply(vs: List[String]): CsvValue =
    Wrapper(vs)
}



/*
sealed trait CsvValue extends Any
{
  def :+(other: CsvValue): CsvValue

  def toCsvString(implicit del: Delimiter.Value): String
}

private case class Values(
  val vs: List[String]
)
extends AnyVal
with CsvValue
{

  def :+(other: CsvValue) =
    other match {
      case Values(vprs)   => Values(vs :++ vprs)
      case SingleValue(v) => Values(vs :+ v)
      case Empty          => Values(vs :+ "")
    }

  override def toCsvString(implicit del: Delimiter.Value) =
    vs.mkString(del.toString)
}

private case class SingleValue(
  val v: String
)
extends AnyVal
with CsvValue
{
  def :+(other: CsvValue) =
    other match {
      case Values(vprs)     => Values(List(v) :++ vprs)
      case SingleValue(vpr) => Values(List(v) :+ vpr)
      case Empty            => Values(List(v))
    }

  override def toCsvString(implicit del: Delimiter.Value) = v
}

private case object Empty extends CsvValue
{
  def :+(other: CsvValue) =
    other match {
      case Values(vprs)     => Values(vprs)
      case SingleValue(vpr) => Values(List(vpr))
      case Empty            => Values(List(""))
    }

  override def toCsvString(implicit del: Delimiter.Value) = ""
}
*/
