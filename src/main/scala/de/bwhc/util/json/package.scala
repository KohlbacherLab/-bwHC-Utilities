package de.bwhc.util



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
