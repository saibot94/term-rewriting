package com.cristis.functions

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by darkg on 5/2/2017.
  */
class TermTest extends WordSpec with Matchers {
  val testBookTerm = Fct("f", List(Constant("e"),
        Fct("f", List(Variable("x"), Fct("i", List(Variable("x")))))))
  "Term.pos" when {
    "getting the position of a symbol" should {
      "return empty string set for variable or constant" in {
        Variable("x").pos() shouldBe Set("")
        Constant("x").pos() shouldBe Set("")
      }

      "return symbols for correct expression" in {
        Fct("f", List(Variable("x"), Variable("y"))).pos() shouldBe Set("", "1", "2")
      }

      "return symbols for larger correct expression " in {
        Fct("f",
          List(Constant("e"),
            Fct("f", List(Variable("x"), Fct("i", List(Variable("x"))))))).pos() shouldBe Set("", "1", "2", "21", "22", "221")
      }
    }

  }

  "Term.size" when {
    "getting the size of the position set" should {
      "return 1" in {
        Variable(1.toString).size shouldBe 1
        Constant(1.toString).size shouldBe 1
      }

      "return more" in {
        Fct("f", List(Variable("x"), Variable("y"))).size shouldBe 3

        Fct("f",
          List(Constant("e"),
            Fct("f", List(Variable("x"), Fct("i", List(Variable("x"))))))).size shouldBe 6

      }

      "verify equality " in {
        testBookTerm shouldBe testBookTerm
      }
    }
  }

  "Term.subterm" when {
    "getting the empty subterm of anything" should {
      "yield that" in {
        Variable("x").subterm("") shouldBe Variable("x")
      }
    }
    "getting the term from a specific position" should {
      "get that term" in {
        Fct("f", List(Variable("x"), Variable("y"))).subterm("1") shouldBe Variable("x")
      }

      "get the more complicated terms" in {
        val expr = Fct("f",
          List(Constant("e"),
            Fct("f", List(Variable("x"), Fct("i", List(Variable("x")))))))

        expr.subterm("22") shouldBe Fct("i", List(Variable("x")))
        expr.subterm("221") shouldBe Variable("x")
        expr.subterm("1") shouldBe Constant("e")
        expr.subterm("") shouldBe expr
      }
    }
  }

  "Term.replace" when {
    "replacing epsilon pos" should {
      "replace whole term" in {
        val toReplace = Fct("f", List(Variable("b"), Variable("a")))
        Variable("x").replace("", toReplace) shouldBe toReplace
      }
      "replace whole term in bigger expression" in {
        val toReplace = Fct("f", List(Variable("b"), Variable("a")))
        val added = Fct("f", List(Variable("bla")))
        val expected = Fct("f", List(added, Variable("a")))

        toReplace.replace("1", added) shouldBe expected
      }

      "replace in larger exp" in {
        val added = Fct("f", List(Variable("a"), Constant("b")))
        val testBookTermExpected = Fct("f", List(Constant("e"),
          Fct("f", List(Variable("x"), Fct("i", List(added))))))

        testBookTerm.replace("221", added) shouldBe testBookTermExpected
      }

      "trim larger tree" in {
        testBookTerm.replace("2", Constant("e")) shouldBe Fct("f", List(Constant("e"), Constant("e")))
      }
    }
  }

  "Term.vars" when {
    "finding the vars in a constant function" should {
      "return the empty set" in {
        Fct("f", List(Constant("e"), Constant("e"))).vars shouldBe Set()
      }
      "return the single var" in {
        Fct("f", List(Fct("i", List(Variable("x"))), Constant("e"))).vars shouldBe Set(Variable("x"))
      }
      "return multiple vars" in {
        Fct("f", List(Fct("i", List(Variable("x"), Variable("y"))), Fct("i", List(Variable("y"))))).vars shouldBe
            Set(Variable("x"), Variable("y"))
      }
    }
  }
}
