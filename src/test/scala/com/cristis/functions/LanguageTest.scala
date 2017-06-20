package com.cristis.functions

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by cristian.schuszter on 2017-03-20.
  */
class LanguageTest extends WordSpec with Matchers {

  val testFunctions: List[(String, Int)] = List(("f", 2), ("g", 1))
  val constants: List[String] = List("a", "b", "0")

  "Language.validateInput" when {
    "validating empty input" should {
      "return false" in {
        val lang = new Language(constants, testFunctions)
        lang.validateInput("") shouldBe false
      }
    }
    "validating gibberish" should {
      "return false" in {
        val lang = new Language(constants, testFunctions)
        lang.validateInput("asdasd asd as gfdg ad as") shouldBe false
      }
    }
    "validating simple term" should {
      "return true" in {
        val lang = new Language(constants, testFunctions)
        lang.validateInput("asd") shouldBe true
      }
    }
    "validating thing without matching paranthesis" should {
      val lang = new Language(constants, testFunctions)

      "return false with starting paran" in {
        lang.validateInput("asd(") shouldBe false
      }
      "return false with wrapped parans" in {
        lang.validateInput("(asd)") shouldBe false
      }
      "return false with end paran" in {
        lang.validateInput("asd)") shouldBe false
      }
    }
    "validating correct expression" should {
      val lang = new Language(constants, testFunctions)

      "return true for simple expression " in {
        lang.validateInput("g(0) ".replaceAll(" ", "")) shouldBe true
      }

      "return false for non-valid variable symbol" in {
        lang.validateInput("g(a0&) ".replaceAll(" ", "")) shouldBe false
      }

      "return true for more advanced expression" in {
        lang.validateInput("f(g(b),  g(f(a,b)))".replaceAll(" ", "")) shouldBe true
      }
      "return true for even more advanced expression" in {
        lang.validateInput("f(f(g(b), g(0)), f(0,b))".replaceAll(" ", "")) shouldBe true
      }
    }
  }

  "LanguageTest.build" when {
    "building just a variable expression" should {
      "give the variable" in {
        val lang = new Language(constants, testFunctions)

        val exp = Var("x")
        lang.build("x") shouldBe exp
      }
      "give a function symbol" in {
        val lang = new Language(constants, testFunctions)
        lang.build("f(x,y)") shouldBe Fct("f", List(Var("x", 0), Var("y", 0)))
      }
      "give the correct constant" in {
        val lang = new Language(constants, testFunctions)
        lang.build("a") shouldBe Fct("a")
      }
      "give a more complicated result" in {
        val lang = new Language(constants, testFunctions)
        lang.build("f(x,f(g(y),z))") shouldBe Fct("f", List(Var("x", 0), Fct("f", List(Fct("g", List(Var("y", 0))), Var("z", 0)))))
      }
      "give more complicated result v2" in {
        val lang = new Language(constants, testFunctions)
        lang.build("f(f(g(b), g(0)), f(0,b))".replaceAll(" ", "")) shouldBe
          Fct("f",
            List(Fct("f",
              List(Fct("g", Fct("b") :: Nil),
                Fct("g", Fct("0") :: Nil))),
              Fct("f", Fct("0") :: Fct("b") :: Nil)))
      }
    }
  }
}
