package com.cristis.ordering

import com.cristis.functions.{Fct, Term, Var}

/**
  * Created by chris on 6/19/2017.
  */
object KBO {

  def totalWeight(t: Term): Double = t match {
    case _: Var => 1.0
    case Fct(_, Nil) => 2.0
    case Fct(f, ts) => 1.0 + ts.map(ti => totalWeight(ti)).sum
  }


  def computeKbo(orderingFunc: ((String, String) => Order))(
    s: Term,
    t: Term
  ): Order = {
    if(s.vars.size >= t.vars.size) {
      // KBO1
      if(totalWeight(s) > totalWeight(t)) {
        GR
        // KBO2
      } else if(totalWeight(s) == totalWeight(t)) {
        (s,t) match {
          //KBO2a - unary function applied n times, t == x
          case (Fct(f, si :: Nil), _) if kbo2aUnfold(f, s, t) => GR
          case (Fct(f, si :: Nil), _) if !kbo2aUnfold(f, s, t) => NGE
          //KBO2b - root symbols bigger
          case (Fct(f, ss), Fct(g, ts)) if orderingFunc(f, g) == GR => GR
          //KBO2c - recursive call
          case (Fct(f, ss), Fct(g, ts)) if orderingFunc(f, g) == EQ =>
            if(ss.zip(ts).exists{ case (si, ti) => computeKbo(orderingFunc)(si, ti) == GR }) {
              GR
            } else {
              NGE
            }
          case _ => NGE
        }
      } else {
        NGE
      }
    } else {
      NGE
    }
  }

  private def kbo2aUnfold(f: String, s: Term, t: Term): Boolean = s match {
    case Fct(sym, si :: Nil) if sym == f => kbo2aUnfold(f, si, t)
    case x: Var => x == t
    case _ => false
  }
}
