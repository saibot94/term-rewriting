package com.cristis.completion

import com.cristis.TermRewritingSystem.TRS
import com.cristis.critpairs.CriticalPair
import com.cristis.functions.{Substitutions, Term}
import com.cristis.ordering._
import com.cristis.unification.{Norm, Unifier}

import scala.util.{Success, Try}

/**
  * Created by cristian.schuszter on 6/19/2017.
  */
class SimpleCompletion(ordering: List[String], equalities: List[(Term, Term)]) {

  import LexicographicPathOrdering._

  var oldR: TRS = null
  var newR: TRS = null

  private def orderingFunc(s1: String, s2: String): Order = {
    val ord1 = ordering.indexOf(s1)
    val ord2 = ordering.indexOf(s2)
    if (ord1 == ord2) {
      EQ
    } else if (ord1 > ord2) {
      NGE
    } else {
      GR
    }
  }

  def validate: Boolean = {
    equalities.forall { case (left, right) =>
      if (left == right) {
        true
      } else {
        val canOrder = (computeLpo(orderingFunc)(left, right) == GR) ||
          (computeLpo(orderingFunc)(right, left) == GR)
        if (!canOrder) {
          throw new CompletionException(s"Unable to validate initial system! Cannot orient $left and $right from E")
        }
        canOrder
      }
    }
  }

  private def buildInitialTrs: TRS = {
    equalities.filterNot { case (left, right) => left == right }.map {
      case (left, right) =>
        orientTerms(left, right)
    }
  }


  def solve: TRS = {
    if (!validate) {
      throw new CompletionException(s"Unable to validate initial system! Validation procedure returned false")
    }
    oldR = buildInitialTrs
    newR = null
    var i = 1
    while (oldR != newR) {
      if (newR == null) {
        newR = oldR
      }
      oldR = newR
      val critPairs = CriticalPair.criticalPairs(oldR)
      // Normalize all of the s, ts from the resulting rules
      val normalizedCritPairs = critPairs.map {
        case (left, right) =>
          (Norm.norm(oldR, left), Norm.norm(oldR, right))
        }
        .filterNot { case (left, right) =>
          Try {
            Substitutions.lift(Unifier.unify(left, right), left) == right
          } match {
            case Success(un) => un
            case _ => false
          }
        }
      .filterNot { case (left, right) => left == right }.distinct

      // Orient the crit pairs and add them to the new set
      val orientedPairs = normalizedCritPairs.map {
        case (left, right) =>
          orientTerms(left, right)
      }
      newR = oldR ::: orientedPairs
      println(s"system after step $i: ")
      i+=1
      newR.foreach {
        case (left, right) =>
          println(left + " -> " + right)
      }
    }
    newR
  }


  private def orientTerms(left: Term, right: Term) = {
    if (computeLpo(orderingFunc)(left, right) == GR) {
      (left, right)
    } else if (computeLpo(orderingFunc)(right, left) == GR) {
      (right, left)
    } else {
      throw new CompletionException(s"Cannot orient $left and $right from E")
    }
  }

  class CompletionException(message: String) extends Exception(message)

}
