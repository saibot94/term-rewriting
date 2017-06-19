package com.cristis.unification

import com.cristis.TermRewritingSystem.TRS
import com.cristis.functions.Substitutions.Substitution
import com.cristis.functions._

import scala.annotation.tailrec

/**
  * Created by darkg on 5/9/2017.
  */
object Unifier {

  @tailrec
  private final def solve(ts: TRS, s: Substitution): Substitution = ts match {
    case Nil => s
    case (x: Var, t: Term) :: rest => if (x == t) solve(rest, s) else elim(x, t, rest, s)
    case (t: Term, x: Var) :: rest => if (x == t) solve(rest, s) else elim(x, t, rest, s)
    case (f: Fct, g: Fct) :: rest => if (f.symbol == g.symbol) solve(f.children.zip(g.children) ::: rest, s) else throw new UnificationException
  }

  @tailrec
  private final def matchs(ts: TRS, s: Substitution): Substitution = ts match {
    case Nil => s
    case (x: Var, t: Term) :: rest =>
      if (Substitutions.indom(x, s)) {
        if (Substitutions.app(x, s) == t) {
          matchs(rest, s)
        }
        else throw new UnificationException
      } else {
        matchs(rest, Set((x, t)) | s)
      }
    case (_: Term, _: Var) :: _ => throw new UnificationException
    case (t1: Fct, t2: Fct) :: rest =>
      if (t1.symbol == t2.symbol) matchs(t1.children.zip(t2.children) ::: rest, s) else throw new UnificationException

  }

  private def elim(x: Var, t: Term, toSolve: TRS, subst: Substitution): Substitution = {
    if (t.occurs(x)) {
      throw new UnificationException
    }
    else {
      val xt: Substitution = Set((x, t))
      val mappedToSolve = toSolve.map { case (t1, t2) => (Substitutions.lift(xt, t1), Substitutions.lift(xt, t2)) }
      val newSubst: Substitution = xt | subst.map { case (y, u) => (y, Substitutions.lift(xt, u)) }
      solve(mappedToSolve, newSubst)
    }
  }

  /**
    * Unify the lhs and rhs of an expression E, as defined by the Term Rewriting book.
    * This either throws an UnificationException or returns an MGU.
    *
    * @param t1 first term
    * @param t2 second term
    * @return a substitution that transform t1 into t2
    */
  def unify(t1: Term, t2: Term): Substitution = solve(List[(Term, Term)]((t1, t2)), Set())

  /**
    * Pattern match the lhs with the rhs
    *
    * @param pattern the lhs that we want to match to the rhs
    * @param obj     the rhs
    * @return a fitting substitution or throws UnificationException if nothing can be found
    */
  def matchfunc(pattern: Term, obj: Term): Substitution = matchs(List((pattern, obj)), Set())
}

class UnificationException extends Exception {

}
