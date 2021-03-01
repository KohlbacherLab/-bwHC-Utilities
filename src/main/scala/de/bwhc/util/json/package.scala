package de.bwhc.util


import scala.util.{Either, Left, Right, Try}


package object json
{

  import play.api.libs.json._

  import cats.data.NonEmptyList


  implicit def formatNel[T: Reads: Writes](
    implicit
    reads: Reads[List[T]],
    writes: Writes[List[T]]
  ): Format[NonEmptyList[T]] =
    Format[NonEmptyList[T]](
      reads
        .filterNot(JsonValidationError("Found empty list where non-empty list expected"))(_.isEmpty)
        .map(NonEmptyList.fromListUnsafe),
      writes.contramap(_.toList)
    )



  implicit def writesEither[T: Writes, U: Writes]: Writes[Either[T,U]] =
    Writes(
      _.fold(
        Json.toJson(_),
        Json.toJson(_)
      )
    )


  implicit def formatEither[T: Format, U: Format]: Format[Either[T,U]] =
    Format(
      Reads(
        js =>
          js.validate[U].map(Right[T,U](_))
            .orElse(js.validate[T].map(Left[T,U](_)))
      ),
      Writes(
        _.fold(
          Json.toJson(_),
          Json.toJson(_)
        )
      )
    )



  object time
  {

    import java.time.{LocalDate,YearMonth}
    import java.time.format.DateTimeFormatter

    import cats.syntax.either._


    private val yyyyMM = DateTimeFormatter.ofPattern("yyyy-MM")
 
 
    implicit val formatYearMonth: Format[YearMonth] =
      Format(
        Reads(
          js =>
            for {
              s <- js.validate[String]
              result <-
                Try( 
                  YearMonth.parse(s,yyyyMM)
                )
                .orElse(
                  Try(LocalDate.parse(s,DateTimeFormatter.ISO_LOCAL_DATE))
                    .map(d => YearMonth.of(d.getYear,d.getMonth))
                )
                .map(JsSuccess(_))
                .getOrElse(JsError(s"Invalid Year-Month value $s; expected format YYYY-MM (or YYYY-MM-DD as fallback)") )
            } yield result
        ),
        Writes(
          d => JsString(yyyyMM.format(d))
        )
      )

  }


  object hlists {

    object labelled {

      import shapeless.{
        HList, HNil, ::, Lazy, Witness
      }
      import shapeless.labelled.FieldType
  
  
      implicit def writesHNil: Writes[HNil] =
        Writes(hnil => JsObject.empty)

      implicit def writesLabelledHList[K <: Symbol, H, T <: HList](
        implicit 
        witness: Witness.Aux[K],
        hf: Lazy[Writes[H]],
        tf: Writes[T]
      ): Writes[FieldType[K,H] :: T] = 
        Writes {
          case h :: t =>
            Json.obj(witness.value.name -> hf.value.writes(h)) ++ tf.writes(t).as[JsObject]
        }
        

    }

  }



}
