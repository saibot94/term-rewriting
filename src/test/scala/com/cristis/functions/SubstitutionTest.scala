package com.cristis.functions

import org.scalatest.Matchers
import org.scalatest.WordSpec

class SubstitutionTest extends WordSpec with Matchers {
  "Substitution.substitute" when {
    "substituting from one substitution" should {
      "replace one variable" in {
        val subs = Set(
          (Variable("x") -> Fct("i", List(Variable("y")))))

        val sub = new Substitution(subs)
        val s = Fct("f", List(Constant("c"), Variable("x")))
        val t = sub.substitute(s)

        t shouldBe Fct("f", List(Constant("c"), Fct("i", List(Variable("y")))))
      }

      "replace all variables" in {
        val subs = Set(
          (Variable("x") -> Fct("i", List(Variable("y")))))

        val sub = new Substitution(subs)
        val s = Fct("f", List(Constant("c"), Variable("x"), Fct("g", List(Constant("e"), Variable("x")))))
        val t = sub.substitute(s)

        t shouldBe Fct("f", List(Constant("c"), Fct("i", List(Variable("y"))), Fct("g", List(Constant("e"), Fct("i", List(Variable("y")))))))
      }

      "replace all variables at once" in {
        val subs = Set(
          (Variable("x") -> Fct("i", List(Variable("x")))))
        val sub = new Substitution(subs)
        val s = new Fct("f", List(Variable("x"), Variable("x")))
        val t = sub.substitute(s)

        t shouldBe Fct("f", List(Fct("i", List(Variable("x"))), Fct("i", List(Variable("x")))))
      }

    }
    "substituting from multiple sigmas" should {
      val subs = Set(
        Variable("x") -> Fct("i", List(Variable("y"))),
        Variable("y") -> Constant("e"))
      val substitution = new Substitution(subs)

      "replace as in the example" in {
        val s = Fct("f", List(Constant("e"), Variable("x")))
        substitution.substitute(s) shouldBe Fct("f", List(Constant("e"), Fct("i", List(Variable("y")))))
      }

      "replace as in the more complicated example" in {
        val s = Fct("f", List(Variable("y"), Fct("f", List(Variable("x"), Variable("y")))))
        substitution.substitute(s) shouldBe Fct("f", List(Constant("e"), Fct("f", List(Fct("i", List(Variable("y"))), Constant("e")))))
      }
    }
  }
}
