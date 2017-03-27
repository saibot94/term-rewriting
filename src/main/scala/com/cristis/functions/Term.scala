package com.cristis.functions

/**
  * Created by darkg on 3/28/2017.
  */
abstract class Term {
  def name: String
}


case class Fct(name: String, label: Array[Int], children: Array[Term]) extends Term { }
case class Constant(name: String) extends Term{ }
case class Variable(name: String) extends Term{ }