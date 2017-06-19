package com.cristis.ordering

import com.cristis.functions.{Fct, Var}
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by chris on 6/19/2017.
  */
class KBOTest extends WordSpec with Matchers {

  // i > f > e
  val ordering: List[String] = List("i", "f", "e")

  def fctOrdering(s1: String, s2: String): Order = {
    val ord1 = ordering.indexOf(s1)
    val ord2 = ordering.indexOf(s2)
    if (ord1 == ord2) {
      EQ
    } else if (ord1 > ord2) {
      NGE
    } else {
      GR
    }
  }

  "LexicographicPathOrdering.computeLpo" when {
    "comparing a term and a var" should {
      "compute correctly LPO1" in {
        val larger = Fct("f", List(Var("x"), Fct("e")))
        val smaller = Var("x")
        LexicographicPathOrdering.computeLpo(fctOrdering)(larger, smaller) shouldBe GR
      }
    }
    "comparing more complicated cases" should {
      "compute correctly LPO2a" in {
        val larger = Fct("i", List(Fct("e")))
        val smaller = Fct("e")
        LexicographicPathOrdering.computeLpo(fctOrdering)(larger, smaller) shouldBe GR
      }
      "partially finish example for LPO2c" in {
        val larger = Fct("i", List(Fct("f", List(Var("x"), Var("y")))))
        val smaller = Fct("i", List(Var("x")))
        LexicographicPathOrdering.computeLpo(fctOrdering)(larger, smaller) shouldBe GR
      }
      "return GR in the book's more complicated LPO2c case" in {
        val smaller = Fct("f", List(Fct("i", List(Var("x"))), Fct("i", List(Var("y")))))
        val greater = Fct("i", List(Fct("f", List(Var("x"), Var("y")))))
        LexicographicPathOrdering.computeLpo(fctOrdering)(greater, smaller) shouldBe GR
      }
      "return GR for another example" in {
        val greater = Fct("f", List(Fct("f", List(Var("x"), Var("y"))), Var("z")))
        val smaller = Fct("f", List(Var("x"), Fct("f", List(Var("y"), Var("z")))))
        LexicographicPathOrdering.computeLpo(fctOrdering)(greater, smaller) shouldBe GR
      }
    }
  }
}
