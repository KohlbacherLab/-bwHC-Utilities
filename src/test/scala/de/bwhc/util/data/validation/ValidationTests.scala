package de.bwhc.util.data.validation


import org.scalatest.flatspec.AnyFlatSpec

import de.bwhc.util.data.validation.dsl
import de.bwhc.util.data.validation.dsl._
import cats.instances.list._


class ValidationTests extends AnyFlatSpec
{

  "Option validations" must "have been correct" in {

    assert((Some(42) must be (defined)).isValid)

    assert((Some(42) must be (defined)).isValid)

    assert((None must be (undefined)).isValid)

    assert((None must not (be (defined))).isValid)

    assert((Option(42) must be (a [Some[Int]])).isValid)

    assert((Option(null) mustBe a [None.type]).isValid)

    assert((Option(null) must (be (an [None.type]))).isValid)

    assert((null.asInstanceOf[Any] must (be (undefined))).isValid)

    assert((42 must (be (defined))).isValid)

    assert((Some(42) must contain (42)).isValid)

    assert((Some(42) must not (contain (24))).isValid)

    assert((Some(42).toRight(0) must contain (42)).isValid)


  }


  "List validations" must "have been correct" in {

    assert((List.empty must be (empty)).isValid)

    assert((List(1,2,3,4) must not (be (empty))).isValid)

    assert((List.empty must have (size (0))).isValid)

    assert((List(1,2,3,4) must not (have (size (0)))).isValid)

    assert(List(1,2,3,4).validateEach(positive[Int]).isValid)

    assert((42 must be (in (List(1,2,42)))).isValid)

    assert((13 must not (be (in (List(1,2,42))))).isValid)

    assert((List(1,2,3,4) must contain (anyOf(1,2))).isValid)

    assert((List(1,2,3,4) must contain (allOf(1,2,3))).isValid)


  }


  "Numeric validations" must "have been correct" in {

    assert((42 must equal(42)).isValid)

    assert((42 must be (positive[Int])).isValid)

    assert((-42 must be (negative[Int])).isValid)

    assert((-42 must not (be (positive[Int]))).isValid)

    assert((-42 mustNot be (positive[Int])).isValid)

  }



  "String validations" must "have been correct" in {

    assert(("Test String" hasTo have (length (11))).isValid)

    assert(("Test String" hasTo contain("est")).isValid)

    assert(("Test String" hasTo contain('t')).isValid)

    assert(("Test String" hasTo contain (anyOf("ettt","ing"))).isValid)

    assert(("Test String" hasTo contain (allOf("est","t S","ing"))).isValid)

    assert(("Test String" hasTo contain (only ("Test String"))).isValid)

    assert(("Test String" hasTo not (contain (only ("e")))).isValid)

    assert(("Test String" hasTo startWith("Test")).isValid)

    assert(("Test String" hasTo endWith("ring")).isValid)

    assert(("Test String" hasTo matchRegex("Test String")).isValid)


  }


}
