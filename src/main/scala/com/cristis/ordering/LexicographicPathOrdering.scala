package com.cristis.ordering

import com.cristis.functions.{Fct, Term, Var}

/**
  * Created by chris on 6/17/2017.
  */

/**
  * This class defines lexicographic path orderings as defined in the Term Rewriting book
  */
object LexicographicPathOrdering {

  /**
    * Definition >lpo as in the term rewriting book
    *
    * @param orderingFunc the function that can compare two function symbols
    *                     and decide which one of them is bigger
    * @param s            the lhs of the ordering
    * @param t            the rhs of the ordering
    * @return the result, one of the three possible outcomes: Not greater or equal,
    *         Greater and Equal (NGE, GR, EQ)
    */
  def computeLpo(orderingFunc: ((String, String) => Order))(
                 s: Term,
                 t: Term
                ): Order = {
    (s, t) match {
      // This is LPO1
      case (_, x: Var) =>
        if (s == t) {
          EQ
        } else if (s.occurs(x)) {
          GR
        }
        else NGE
      case (_: Var, _: Term) => NGE
      // LPO2, which splits into 3 children
      case (Fct(f, ss), Fct(g, ts)) =>
        val allSubtermsSmaller = ss.forall(si => computeLpo(orderingFunc)(si, t) == NGE)
        if (allSubtermsSmaller) {
          orderingFunc(f, g) match {
            // LPO2b
            case GR =>
              val greaterThanSubterms = ts.forall(ti => computeLpo(orderingFunc)(s, ti) == GR)
              if (greaterThanSubterms) {
                GR
              } else {
                NGE
              }
            // LPO2c
            case EQ =>
              val greaterThanSubterms = ts.forall(ti => computeLpo(orderingFunc)(s, ti) == GR)
              if (greaterThanSubterms) {
                lex(computeLpo(orderingFunc), (ss, ts))
              } else {
                NGE
              }
            case NGE => NGE
          }
        } else {
          // LPO2a
          GR
        }
    }
  }

  private def lex(ord: ((Term, Term) => Order), pair: (List[Term], List[Term])): Order = pair match {
    case (Nil, Nil) => EQ
    case (x::xs, y::ys) =>
      ord(x, y) match {
        case GR => GR
        case EQ => lex(ord, (xs,ys))
        case NGE => NGE
      }
  }

}

trait Order

case object NGE extends Order

case object GR extends Order

case object EQ extends Order
