package com.cristis.completion

import com.cristis.functions.{Fct, Var}
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by cristian.schuszter on 6/19/2017.
  */
class SimpleCompletionTest extends WordSpec with Matchers {

  val equalities = List(
    Fct("*", List(Fct("*", List(Var("x"), Var("y"))), Var("z"))) -> Fct("*", List(Var("x"), Fct("*", List(Var("y"), Var("z"))))),
    Fct("*", List(Fct("i", List(Var("x"))), Var("x"))) -> Fct("1"),
    Fct("*", List(Fct("1"), Var("x"))) -> Var("x")
  )

  val eqSimple = List (
    Fct("*", List(Fct("*", List(Var("x"), Var("y"))), Fct("*", List(Var("y"), Var("z"))))) -> Var("y")
  )

  "SimpleCompletion.solve" when {
    "trying to solve a simple example" should {
      "solve it in a step" in {
        val orderings = List("*")
        val completionObj = new SimpleCompletion(orderings, eqSimple)
        println(completionObj.solve)
      }
    }
//
//    "trying to solve the book example" should {
//      "return the needed things" in {
//        val orderings = List("i", "*", "1").permutations.toList
//        orderings.foreach { ord =>
//          println(s"Trying order: $ord")
//          val completionObj = new SimpleCompletion(ord, equalities)
//          try {
//            println(completionObj.solve)
//            println(s"System solvable for $ord")
//          } catch  {
//            case e: Exception =>
//              println(s"$e")
//          }
//
//        }
//
////        val orderings = List("i", "*", "1")
////        val completionObj = new SimpleCompletion(orderings, equalities)
////        println(completionObj.solve)
//      }
//    }
  }
}
