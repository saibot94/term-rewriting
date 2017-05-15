package com.cristis.functions

import com.cristis.unification.{UnificationException, Unifier}
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by darkg on 5/15/2017.
  */
class UnificationTest extends WordSpec with Matchers {
  "Unifier.unify" when {
    "unifying a simple expression" should {
      "give out the mgu" in {
        val lhs = Var("x")
        val rhs = Fct("f", List(Var("a")))

        val res = Unifier.unify(lhs, rhs)

        res shouldBe Set(Var("x") -> Fct("f", List(Var("a"))))
      }
      "throw an exception if it can't find an mgu" in {
        val lhs = Fct("f", List(Var("x")))
        val rhs = Fct("g", List(Var("y")))

        intercept[UnificationException] { Unifier.unify(lhs, rhs) }
      }
    }
  }
}
