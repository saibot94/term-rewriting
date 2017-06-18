package com.cristis.critpairs

import com.cristis.TermRewritingSystem.TRS
import com.cristis.functions.{Fct, Substitutions, Term, Var}
import com.cristis.unification.Unifier

import scala.util.{Success, Try}

/**
  * Created by chris on 6/18/2017.
  */
object CriticalPair {
  import CritPairUtils._

  def criticalPairs(trs: TRS): List[(Term, Term)] = {
    criticalPairs2(trs, trs)
  }

  def criticalPairs2(r1: TRS, r2: TRS): List[(Term, Term)] = {
    r2.map(r => computeCps(r1, r)).reduceLeft(_ ::: _)
  }

  def computeOnePair(context: Term => Term,
                     pair1: (Term, Term),
                     pair2: (Term, Term)): List[(Term, Term)] = {
    import Substitutions._
    Try {
      val unified = Unifier.unify(pair1._1, pair2._1)
      List((lift(unified, pair1._2),lift(unified, context(pair2._2))))
    } match {
      case Success(x)=> x
      case _ => Nil
    }
  }

  def computeCps(rewriteSystem: TRS,  pair: (Term, Term)): List[(Term, Term)] = {

    def cps(context: Term => Term, pair: (Term, Term)): List[(Term, Term)] =
      pair match {
        case (_: Var, _) => Nil
        case (Fct(f, ts), r: Term) =>
          rewriteSystem.flatMap(rule => computeOnePair(context, (Fct(f, ts), r), rule)) ::: innercps(context, (f, Nil, ts, r))

      }

    def innercps(termToTerm: (Term) => Term, tuple: (String, List[Term], List[Term], Term)): List[(Term, Term)] = {

      tuple match {
        case (_, _, Nil, _) => Nil
        case (f, ts0, t::ts1, r) =>
          def cf(s: Term): Term = termToTerm(Fct(f, ts0 ::: List(s) ::: ts1))
          cps(cf, (t, r)) ::: innercps(termToTerm, (f, ts0 ::: List(t), ts1, r))
      }
    }

    val m = maxs(rewriteSystem.map { case (left, right) => max(maxindex(left), maxindex(right))} ) + 1
    cps((t: Term) => t, (rename(m, pair._1), rename(m, pair._2)))
  }


}

