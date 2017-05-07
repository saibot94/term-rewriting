package com.cristis.functions

import scala.annotation.tailrec

/**
 * Created by darkg on 3/28/2017.
 */
abstract class Term {
  type Identity = (Term, Term)

  def symbol: String

  def pos(prev: String = ""): Set[String] = {
    this match {
      case Constant(_) => Set(prev)
      case Variable(_) => Set(prev)
      case Fct(_, children) => children.zipWithIndex.map(s => (s._1, s._2 + 1)).map {
        case (c, i) =>
          Set(prev) | c.pos(prev + i.toString)
      }.reduceLeft((a, b) => a | b)
    }
  }

  def size: Int = pos().size

  @tailrec
  final def subterm(p: String): Term = p match {
    case "" => this
    case str => this match {
      case Fct(_, children) => children(str.take(1).toInt - 1).subterm(str.tail)
      case Variable(_) | Constant(_) => throw new IllegalArgumentException("Invalid position")
    }
  }

  def replace(position: String, t: Term): Term = position match {
    case "" => t
    case str => this match {
      case Fct(symbol, children) =>
        val childPos = str.take(1).toInt - 1
        val toChange = children(childPos)
        Fct(symbol, children.take(childPos) ++ List(toChange.replace(str.tail, t)) ++ children.drop(childPos + 1))
      case Variable(_) | Constant(_) => throw new IllegalArgumentException("Invalid position")
    }
  }

  def vars: Set[(Variable, String)] = {
    pos().map { p =>
      this.subterm(p) match {
        case s: Variable => (s, p)
        case _ => null
      }
    }.filterNot(v => v == null)
  }

  def ground: Boolean = vars.isEmpty

  def same(other: Term): Boolean = this == other
}

case class Fct(symbol: String, children: List[Term]) extends Term {}

case class Constant(symbol: String) extends Term {}

case class Variable(symbol: String) extends Term {}

class TermBuilder(expr: String, lang: Language) {

}
