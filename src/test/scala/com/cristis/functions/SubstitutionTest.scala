package com.cristis.functions

import org.scalatest.Matchers
import org.scalatest.WordSpec

class SubstitutionTest extends WordSpec with Matchers {
  "Substitution.substitute" when {
    "substituting from one substitution" should {
      "replace one variable" in {
        val subs = Set(
          Var("x") -> Fct("i", List(Var("y"))))

        val sub = new Substitution(subs)
        val s = Fct("f", List(Fct("c"), Var("x")))
        val t = sub.lift(s)

        t shouldBe Fct("f", List(Fct("c"), Fct("i", List(Var("y")))))
      }

      "replace all variables" in {
        val subs = Set(
          Var("x") -> Fct("i", List(Var("y"))))

        val sub = new Substitution(subs)
        val s = Fct("f", List(Fct("c"), Var("x"), Fct("g", List(Fct("e"), Var("x")))))
        val t = sub.lift(s)

        t shouldBe Fct("f", List(Fct("c"), Fct("i", List(Var("y"))), Fct("g", List(Fct("e"), Fct("i", List(Var("y")))))))
      }

      "replace all variables at once" in {
        val subs = Set(
          Var("x") -> Fct("i", List(Var("x"))))
        val sub = new Substitution(subs)
        val s = new Fct("f", List(Var("x"), Var("x")))
        val t = sub.lift(s)

        t shouldBe Fct("f", List(Fct("i", List(Var("x"))), Fct("i", List(Var("x")))))
      }

    }
    "substituting from multiple sigmas" should {
      val subs = Set(
        Var("x") -> Fct("i", List(Var("y"))),
        Var("y") -> Fct("e"))
      val substitution = new Substitution(subs)

      "replace as in the example" in {
        val s = Fct("f", List(Fct("e"), Var("x")))
        substitution.lift(s) shouldBe Fct("f", List(Fct("e"), Fct("i", List(Var("y")))))
      }

      "replace as in the more complicated example" in {
        val s = Fct("f", List(Var("y"), Fct("f", List(Var("x"), Var("y")))))
        substitution.lift(s) shouldBe Fct("f", List(Fct("e"), Fct("f", List(Fct("i", List(Var("y"))), Fct("e")))))
      }
    }
  }

  "Substitution.indom" when {
    val subs = Set(
      Var("y") -> Fct("i", List(Var("y"))),
      Var("x") -> Fct("e"))

    "verifying if indom" should {
      "return true" in {
        val x = Var("x")
        val substitution = new Substitution(subs)

        substitution.indom(x) shouldBe true
      }

      "return false" in {
        val x = Var("z")
        val substitution = new Substitution(subs)

        substitution.indom(x) shouldBe false
      }
    }
  }

  "Substitution.app" when {
    val subs = Set(
      Var("y") -> Fct("i", List(Var("y"))),
      Var("x") -> Fct("e"))

    "applying to indom element" should {
      "return the resulting term" in {
        val x = Var("x")
        val y = Var("y")
        val substitution = new Substitution(subs)

        substitution.app(x) shouldBe Fct("e")
        substitution.app(y) shouldBe Fct("i", List(Var("y")))

      }
    }
  }

  "Substitution.lift" when {
    "lifting something" should {
      "replace all" in {
        val subs = Set(
          Var("x") -> Fct("i", List(Var("y"))))

        val sub = new Substitution(subs)
        val s = Fct("f", List(Fct("c"), Var("x"), Fct("g", List(Fct("e"), Var("x")))))
        val t = sub.lift(s)

        t shouldBe Fct("f", List(Fct("c"), Fct("i", List(Var("y"))), Fct("g", List(Fct("e"), Fct("i", List(Var("y")))))))
      }
    }
  }
}
