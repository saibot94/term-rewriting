package com.cristis.cirtpairs

import com.cristis.TermRewritingSystem.TRS
import com.cristis.critpairs.CriticalPair
import com.cristis.functions.{Fct, Language, Var}
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by chris on 6/18/2017.
  */
class CriticalPairTest extends WordSpec with Matchers {
  val testFunctions: List[(String, Int)] = List(("f", 2), ("i", 1))
  val constants: List[String] = List("e")
  val lang = new Language(constants, testFunctions)

  "CriticalPair.computeCriticalPair" when {
    "running on a simple example" should {
      "do something" in {
        val testTrs: TRS = List(
          lang.build("f(f(x,y),z)") -> lang.build("f(x,f(y,z))"),
          lang.build("f(i(x),x)") -> lang.build("e")
        )
        println(testTrs)
        val critPairs = CriticalPair.criticalPairs(testTrs)
        println(critPairs)
      }
    }
  }
}
