package com.cristis.functions

import scala.annotation.tailrec

object Substitutions {
  type Substitution = Set[(Var, Term)]

  @tailrec
  final def app(x: Var, substitution: Substitution): Term =
    if (substitution.head._1.symbol == x.symbol) substitution.head._2 else app(x, substitution.tail)

  def indom(x: Var, s: Substitution): Boolean = s.map(_._1).contains(x)

  /**
    * Replaces all occurences of the variables in this substitution in one go
    * @param t Term
    * @return Term
    */
  def lift(s: Substitution, t: Term): Term = t match {
    case x: Var if indom(x, s)  => app(x, s)
    case x: Var => x
    case Fct(f, ts) => Fct(f, ts.map { x => lift(s, x) })
  }
}
