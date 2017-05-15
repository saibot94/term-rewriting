package com.cristis.unification

import com.cristis.functions.{Fct, Substitutions, Term, Var}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Created by darkg on 5/15/2017.
  */
object Norm {

  @tailrec
  private final def rewrite(trs: List[(Term, Term)], t: Term): Term = trs match {
    case Nil => throw new NormException
    case ((l, r) :: rest) => Try { Substitutions.lift(Unifier.matchfunc(l, r), r) } match {
      case Success(res) => res
      case Failure(_) => rewrite(rest, t)
    }
  }

  def norm(trs: List[(Term, Term)], t: Term): Term = t match {
    case x: Var => x
    case Fct(f, children) =>
      val u = Fct(f, children.map(child => norm(trs, child)))
      Try { norm(trs, rewrite(trs, u)) } match {
        case Success(res) => res
        case Failure(_) => u
      }
  }
}

class NormException extends Exception
