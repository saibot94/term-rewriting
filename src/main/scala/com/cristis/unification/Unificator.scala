package com.cristis.unification

import com.cristis.functions.{Substitution, Term, Var}

/**
  * Created by darkg on 5/9/2017.
  */
object Unificator {

  def solve[T](ts: List[(Term, Term)], s: Substitution[T]): Substitution[T] = ts match {
    case Nil => s
    case (x: Var, t: Term) :: rest => if (x == t) solve(rest, s) else elim(x, t, rest, s)
    case (t: Term, x: Var) :: rest => if (x == t) solve(rest, s) else elim(x, t, rest, s)
  }

  def elim[T](variable: Var, t: Term, toSolve: List[(Term, Term)], subst: Substitution[T]): Substitution[T] = {
    if (t.occurs(variable)) {
      throw new UnificationException
    }
    else {
      val xt = subst.lift()
    }
  }

}

class UnificationException extends Exception {

}
