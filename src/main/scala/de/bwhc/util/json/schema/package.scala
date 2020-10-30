package de.bwhc.util.json


import java.net.URI

import json._
import com.github.andyglow.json.Value
import com.github.andyglow.jsonschema.AsPlay._
import json.schema.Version._
import play.api.libs.json.{Json => PlayJson, JsValue}


package object schema {



  def JsValueSchema[T](implicit schema: Schema[T]): JsValue =
    schema.asPlay(Draft04())
//    schema.asPlay(Draft07(""))



  def enumSchema[E <: Enumeration](
    e: E,
    definition: Option[String] = None
  ): Schema[E#Value] = {
    val sch =
      Schema.enum(e.values.map(_.toString)
        .toSet
        .map(Value.str(_))
      )
    definition.map(sch(_)).getOrElse(sch(e.toString))
  }


  implicit def enumToSchema[E <: Enumeration](
    implicit w: shapeless.Witness.Aux[E]
  ): Schema[E#Value] =
    enumSchema(w.value)


  def const[T](t: T): Schema[T] =
    Schema.enum(Set(Value.str(t.toString)))


/*
  import Schema.`object`.Field

  implicit def codingSystemSchema[T](
    implicit system: Coding.System[T]
  ): Schema[Coding.System[T]] =
    const(system)


  implicit def codingSchema[T](
    implicit
    codeSch: Schema[T],
    systemSch: Schema[Coding.System[T]]
  ): Schema[Coding[T]] = {
    val sch =
      Schema.`object`(
        Field("code", codeSch),
        Field("display", Json.schema[String], false),
        Field("system", systemSch),
        Field("version", Json.schema[String], false)
      )
    sch(s"coding-${codeSch.refName.get}")
  }



  implicit val geneSchema = 
   Json.schema[Gene]("gene-name")


  implicit val patientSchema = Json.schema[Patient]



  implicit val ngsReportSchema = Json.schema[SomaticNGSReport]
*/


}
