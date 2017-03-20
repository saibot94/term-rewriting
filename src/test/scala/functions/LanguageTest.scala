package functions

import com.cristis.functions.Language
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
    "validating thing without paranthesis" should {
      "return false" in {
        val lang = new Language(constants, testFunctions)
        lang.validateInput("asd") shouldBe false
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

      "return true for more advanced expression" in {
        lang.validateInput("f(g(b),  g(f(a,b)))".replaceAll(" ", "")) shouldBe true
      }
      "return true for even more advanced expression" in {
        lang.validateInput("f(f(g(b), g(0)), f(0,b))".replaceAll(" ", "")) shouldBe true
      }
    }
  }
}

