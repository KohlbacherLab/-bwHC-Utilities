package de.bwhc.util.data



import org.scalatest.flatspec.AnyFlatSpec


import Validators._


final case class Foo
(
  integer: Int,
  double: Option[Double],
  string: String 
)


class ValidatorTests extends AnyFlatSpec
{
/*
  implicit val fooValidator: Validator[Foo] =
    (
      (equal(31) otherwise "Foo.integer not equal to 31") ~
      (isDefined[Double] otherwise "Foo.double undefined") ~
      equal("foo")
    )(
      unlift(Foo.unapply),
      Foo.apply _
    )



  "Validating Foo" must "fail" in {

    val foo =
      Foo(
        42,
        None,
       "bar"
      )

    println(fooValidator(foo))

    assert(fooValidator(foo).isInvalid)

  }
*/

}
