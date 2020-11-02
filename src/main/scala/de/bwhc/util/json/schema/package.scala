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


  private def enumSchema[E <: Enumeration](
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


  object workarounds
  { 

  // Introduced as workaround for the fact that in derivation for Schema[Set[T]],
  // Schema[T] is NOT included in definitions
  implicit def setSchema[T](
    implicit tSch: Schema[T]
  ): Schema[Set[T]] =
    tSch.refName
      .map(d => Json.schema[Array[T]](s"Set[$d]"))
      .getOrElse(Json.schema[Array[T]])
      .asInstanceOf[Schema[Set[T]]]
      .withValidation(Validation.uniqueItems := true)

  }


  def const[T](t: T): Schema[T] =
    Schema.enum(Set(Value.str(t.toString)))


}
