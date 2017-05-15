package com.cristis.functions

import com.cristis.functions.Substitutions.Substitution
import org.scalatest.Matchers
import org.scalatest.WordSpec

class SubstitutionTest extends WordSpec with Matchers {
  "Substitution.substitute" when {
    "substituting from one substitution" should {
      "replace one variable" in {
        val subs: Substitution = Set(
          Var("x") -> Fct("i", List(Var("y"))))

        val s = Fct("f", List(Fct("c"), Var("x")))
        val t = Substitutions.lift(subs, s)

        t shouldBe Fct("f", List(Fct("c"), Fct("i", List(Var("y")))))
      }

      "replace all variables" in {
        val subs: Substitution = Set(
          Var("x") -> Fct("i", List(Var("y"))))

        val s = Fct("f", List(Fct("c"), Var("x"), Fct("g", List(Fct("e"), Var("x")))))
        val t = Substitutions.lift(subs, s)

        t shouldBe Fct("f", List(Fct("c"), Fct("i", List(Var("y"))), Fct("g", List(Fct("e"), Fct("i", List(Var("y")))))))
      }

      "replace all variables at once" in {
        val subs: Substitution = Set(
          Var("x") -> Fct("i", List(Var("x"))))
        val s = Fct("f", List(Var("x"), Var("x")))
        val t = Substitutions.lift(subs, s)

        t shouldBe Fct("f", List(Fct("i", List(Var("x"))), Fct("i", List(Var("x")))))
      }

    }
    "substituting from multiple sigmas" should {
      val subs: Substitution = Set(
        Var("x") -> Fct("i", List(Var("y"))),
        Var("y") -> Fct("e"))

      "replace as in the example" in {
        val s = Fct("f", List(Fct("e"), Var("x")))
        Substitutions.lift(subs, s) shouldBe Fct("f", List(Fct("e"), Fct("i", List(Var("y")))))
      }

      "replace as in the more complicated example" in {
        val s = Fct("f", List(Var("y"), Fct("f", List(Var("x"), Var("y")))))
        Substitutions.lift(subs, s) shouldBe Fct("f", List(Fct("e"), Fct("f", List(Fct("i", List(Var("y"))), Fct("e")))))
      }
    }
  }

  "Substitution.indom" when {
    val subs: Substitution = Set(
      Var("y") -> Fct("i", List(Var("y"))),
      Var("x") -> Fct("e"))

    "verifying if indom" should {
      "return true" in {
        val x = Var("x")


        Substitutions.indom(x, subs) shouldBe true
      }

      "return false" in {
        val x = Var("z")


        Substitutions.indom(x, subs)  shouldBe false
      }
    }
  }

  "Substitution.app" when {
    val subs: Substitution = Set(
      Var("y") -> Fct("i", List(Var("y"))),
      Var("x") -> Fct("e"))

    "applying to indom element" should {
      "return the resulting term" in {
        val x = Var("x")
        val y = Var("y")

        Substitutions.app(x, subs) shouldBe Fct("e")
        Substitutions.app(y, subs) shouldBe Fct("i", List(Var("y")))

      }
    }
  }

  "Substitution.lift" when {
    "lifting something" should {
      "replace all" in {
        val subs: Substitution = Set(
          Var("x") -> Fct("i", List(Var("y"))))

        val s = Fct("f", List(Fct("c"), Var("x"), Fct("g", List(Fct("e"), Var("x")))))
        val t = Substitutions.lift(subs, s)

        t shouldBe Fct("f", List(Fct("c"), Fct("i", List(Var("y"))), Fct("g", List(Fct("e"), Fct("i", List(Var("y")))))))
      }
    }
  }
}
