package com.cristis.functions

import scala.annotation.tailrec

class Substitution[T <: Term](subs: Set[(Var, T)]) {

  @tailrec
  final def app(x: Var, substitution: Set[(Var, T)] = this.subs): Term =
    if (substitution.head._1.symbol == x.symbol) substitution.head._2 else app(x, substitution.tail)

  def indom(x: Var): Boolean = subs.map(_._1).contains(x)

  /**
    * Replaces all occurences of the variables in this substitution in one go
    * @param t Term
    * @return Term
    */
  def lift(t: Term): Term = t match {
    case x: Var if indom(x)  => app(x, subs)
    case x: Var => x
    case Fct(f, ts) => Fct(f, ts.map(lift))
  }
}
