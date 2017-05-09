package com.cristis.functions

import org.scalatest.{ Matchers, WordSpec }

/**
 * Created by darkg on 5/2/2017.
 */
class TermTest extends WordSpec with Matchers {
  val testBookTerm = Fct("f", List(Fct("e"),
    Fct("f", List(Var("x"), Fct("i", List(Var("x")))))))
  "Term.pos" when {
    "getting the position of a symbol" should {
      "return empty string set for variable or constant" in {
        Var("x").pos() shouldBe Set("")
        Fct("x").pos() shouldBe Set("")
      }

      "return symbols for correct expression" in {
        Fct("f", List(Var("x"), Var("y"))).pos() shouldBe Set("", "1", "2")
      }

      "return symbols for larger correct expression " in {
        Fct("f",
          List(Fct("e"),
            Fct("f", List(Var("x"), Fct("i", List(Var("x"))))))).pos() shouldBe Set("", "1", "2", "21", "22", "221")
      }
    }

  }

  "Term.size" when {
    "getting the size of the position set" should {
      "return 1" in {
        Var(1.toString).size shouldBe 1
        Fct(1.toString).size shouldBe 1
      }

      "return more" in {
        Fct("f", List(Var("x"), Var("y"))).size shouldBe 3

        Fct("f",
          List(Fct("e"),
            Fct("f", List(Var("x"), Fct("i", List(Var("x"))))))).size shouldBe 6

      }

      "verify equality " in {
        testBookTerm shouldBe testBookTerm
      }
    }
  }

  "Term.subterm" when {
    "getting the empty subterm of anything" should {
      "yield that" in {
        Var("x").subterm("") shouldBe Var("x")
      }
    }
    "getting the term from a specific position" should {
      "get that term" in {
        Fct("f", List(Var("x"), Var("y"))).subterm("1") shouldBe Var("x")
      }

      "get the more complicated terms" in {
        val expr = Fct("f",
          List(Fct("e"),
            Fct("f", List(Var("x"), Fct("i", List(Var("x")))))))

        expr.subterm("22") shouldBe Fct("i", List(Var("x")))
        expr.subterm("221") shouldBe Var("x")
        expr.subterm("1") shouldBe Fct("e")
        expr.subterm("") shouldBe expr
      }
    }
  }

  "Term.replace" when {
    "replacing epsilon pos" should {
      "replace whole term" in {
        val toReplace = Fct("f", List(Var("b"), Var("a")))
        Var("x").replace("", toReplace) shouldBe toReplace
      }
      "replace whole term in bigger expression" in {
        val toReplace = Fct("f", List(Var("b"), Var("a")))
        val added = Fct("f", List(Var("bla")))
        val expected = Fct("f", List(added, Var("a")))

        toReplace.replace("1", added) shouldBe expected
      }

      "replace in larger exp" in {
        val added = Fct("f", List(Var("a"), Fct("b")))
        val testBookTermExpected = Fct("f", List(Fct("e"),
          Fct("f", List(Var("x"), Fct("i", List(added))))))

        testBookTerm.replace("221", added) shouldBe testBookTermExpected
      }

      "trim larger tree" in {
        testBookTerm.replace("2", Fct("e")) shouldBe Fct("f", List(Fct("e"), Fct("e")))
      }
    }
  }

  "Term.vars" when {
    "finding the vars in a constant function" should {
      "return the empty set" in {
        Fct("f", List(Fct("e"), Fct("e"))).vars shouldBe Set()
      }
      "return the single var" in {
        Fct("f", List(Fct("i", List(Var("x"))), Fct("e"))).vars.map(_._1).toSet shouldBe Set(Var("x"))
      }
      "return multiple vars" in {
        Fct("f", List(Fct("i", List(Var("x"), Var("y"))), Fct("i", List(Var("y"))))).vars.map(_._1).toSet shouldBe
          Set(Var("x"), Var("y"))
      }
    }
  }

  "Term.parallel" when {
    "checking equalities" should {
      "return true for equal terms" in {
        val first = Fct("f", List(Fct("x"), Var("y")))
        val second = first.copy(first.symbol, first.children)
        first.same(second) shouldBe true

      }

      "return false for non-equals" in {
        val first = Fct("f", List(Fct("x"), Var("y")))
        val second = first.copy(first.symbol, List(Fct("e")))
        first.same(second) shouldBe false

      }
    }
  }
}
