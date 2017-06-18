package com.cristis.console

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import com.cristis.Constants._
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by cristian.schuszter on 2017-03-20.
  */
class ConsolePrompterTest extends WordSpec with Matchers  {

  val myOut: ByteArrayOutputStream  = new ByteArrayOutputStream()
  val constantsPrompter = new ConsolePrompter(ConstantsPrompt, ConstantsInvalidMessage, ConstantsRegex)
  val functionPrompter = new ConsolePrompter(FunctionsPrompt, FunctionsInvalidMessage, FunctionsRegex)


  private def overrideStdInAndOut(inString: String): Unit = {
    System.setIn(new ByteArrayInputStream(inString.getBytes()))
  }

  "ConsolePrompter.read" when {
    "reading a valid option" should {
      "accept it" in {
        overrideStdInAndOut("a\n\n")
        val results = constantsPrompter.read()
        results.length shouldBe 1
        results.head shouldBe "a"
      }
    }
    "reading multiple options" should {
      "read all of them if they are valid" in {
        overrideStdInAndOut("a\nbc\n\n")
        val results = constantsPrompter.read()
        results.length shouldBe 2
        results.head shouldBe "bc"
        results(1) shouldBe "a"
      }
      "skip the invalid ones" in {
        overrideStdInAndOut("a\nnonsense++\nmorecrap_\nbc\n\n")
        val results = constantsPrompter.read()
        results.length shouldBe 2
        results.head shouldBe "bc"
        results(1) shouldBe "a"
      }
    }
    "reading an option that has an invalid character" should {
      "ignore lines with spaces in them" in {
        overrideStdInAndOut("a b\n\n")
        val results = constantsPrompter.read()
        results.length shouldBe 0
      }
      "ignore lines with spaces invalid characters in them" in {
        overrideStdInAndOut("a-=*\nfsdf+\n\n")
        val results = constantsPrompter.read()
        results.length shouldBe 0
      }
      "trim lines with spaces at the start or end" in {
        overrideStdInAndOut("a \n\n")
        val results = constantsPrompter.read()
        results.length shouldBe 1
        results.head shouldBe "a"
      }
    }
    "validating function symbols" should {
      "validate only correct functions" in {
        overrideStdInAndOut("f/3\nbc\n\n")
        val results = functionPrompter.read()
        results.length shouldBe 1
        results.head shouldBe "f/3"
      }
      "invalidate wrong arity functions" in {
        overrideStdInAndOut("f/3\nbc/0\nbc/19\n\n")
        val results = functionPrompter.read()
        results.length shouldBe 1
        results.head shouldBe "f/3"
      }
      "invalidate illegal character functions" in {
        overrideStdInAndOut("f*_/3\nbc/0\nbc_/9\n\n")
        val results = functionPrompter.read()
        results.length shouldBe 0
      }
    }
  }
}
