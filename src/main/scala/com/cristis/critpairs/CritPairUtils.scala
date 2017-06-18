package com.cristis.critpairs

import com.cristis.functions.{Fct, Term, Var}

/**
  * Created by chris on 6/18/2017.
  */
object CritPairUtils {

  def max(i: Int, j: Int): Int = Math.max(i, j)

  def maxs(is: List[Int]): Int =
    is match {
      case i :: iss => max(i, maxs(iss))
      case Nil => 0
    }

  def maxindex(t: Term ): Int = t match {
    case Var(x, i) => i
    case Fct(_, ts) => maxs(ts.map { ti => maxindex(ti)})
  }

  def rename(n: Int, t: Term): Term = t match {
    case Var(x, i) => Var(x, i+n)
    case Fct(f, ts) => Fct(f, ts.map { ti => rename(n, ti) })
  }

}
