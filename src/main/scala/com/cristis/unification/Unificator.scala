package com.cristis.unification

import com.cristis.functions.Substitutions.Substitution
import com.cristis.functions._

import scala.annotation.tailrec

/**
  * Created by darkg on 5/9/2017.
  */
object Unificator {

  @tailrec
  private final def solve(ts: List[(Term, Term)], s: Substitution): Substitution = ts match {
    case Nil => s
    case (x: Var, t: Term) :: rest => if (x == t) solve(rest, s) else elim(x, t, rest, s)
    case (t: Term, x: Var) :: rest => if (x == t) solve(rest, s) else elim(x, t, rest, s)
    case (f: Fct, g: Fct) :: _ => if(f.symbol == g.symbol) solve(f.children.zip(g.children) ::: ts, s) else throw new UnificationException
  }

  private def elim(x: Var, t: Term, toSolve: List[(Term, Term)], subst: Substitution): Substitution = {
    if (t.occurs(x)) {
      throw new UnificationException
    }
    else {
      val xt: Substitution = Set((x, t))
      val mappedToSolve = toSolve.map { case (t1, t2) => (Substitutions.lift(xt, t1), Substitutions.lift(xt, t2)) }
      val newSubst: Substitution = xt | subst.map {case (y, u) => (y, Substitutions.lift(xt, u))}
      solve(mappedToSolve, newSubst)
    }
  }

  def unify(t1: Term, t2: Term): Substitution = solve(List[(Term, Term)]((t1, t2)), Set())
}

class UnificationException extends Exception {

}
