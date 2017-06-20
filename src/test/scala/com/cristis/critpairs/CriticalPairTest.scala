package com.cristis.critpairs

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
        // 6.2 in book, at the examples
        val testTrs: TRS = List(
        Fct("f", List(Fct("f", List(Var("x"), Var("y"))), Var("z"))) -> Fct("f", List(Var("x"), Fct("f", List(Var("y"), Var("z"))))),
        Fct("f", List(Fct("i", List(Var("x", 1))), Var("x", 1))) -> Fct("e")
        )
        val critPairs = CriticalPair.criticalPairs(testTrs)
        critPairs.foreach(println)
      }
    }
  }
}
