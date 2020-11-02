package de.bwhc.util.json.schema


import org.scalatest.flatspec.AnyFlatSpec

import json._

import play.api.libs.json.Json.prettyPrint


object Foo extends Enumeration
{
  type Foo = Value 

  val One,Two, Three, Four = Value
}

final case class Bop
(
  double: Double
)

final case class Bar
(
  int: Int,
  foos: Set[Foo.Value],
  bops: Set[Bop]
)


class Tests extends AnyFlatSpec
{

  import workarounds._


  implicit val bumpSchema = Json.schema[Bop]("Bop")
  implicit val barSchema  = Json.schema[Bar]


  "Bar JSON Schema" should "be valid" in {

    println(prettyPrint(JsValueSchema[Bar]))

  }


}
