package com.cristis.functions

class Substitution[T <: Term](subs: Set[(Variable, T)]) {
  def substitute(term: Term): Term = {
    var newTerm = term
    val intersect = subs.map(_._1).toSet & term.vars.map(_._1)
    val validSubs = subs.filter(s => intersect.contains(s._1))
    val varPos = term.vars.filter(v => intersect.contains(v._1))
    validSubs.foreach {
      f =>
        varPos.filter(v => v._1 == f._1).foreach {
          variable =>
            newTerm = newTerm.replace(variable._2, f._2)
        }
    }
    newTerm
  }
}
