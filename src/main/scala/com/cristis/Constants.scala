package com.cristis

import scala.util.matching.Regex

/**
  * Created by cristian.schuszter on 2017-03-20.
  */
object Constants {
  val ConstantsPrompt: String = "Define a constant symbol and press [ENTER].\nTo end the constants selection, leave the line empty: "
  val ConstantsInvalidMessage: String = "Invalid constant symbol! Use only letters and characters!!"
  val ConstantsRegex: Regex = "^[0-9a-zA-Z]+$".r

  val FunctionsPrompt: String = "Define a function symbol and press [ENTER].\nA function symbol is defined by <name>/<arity>. " +
    "The arity cannot be larger than 9 or smaller than 1. A valid function input is 'f/2' or 'union/2'"
  val FunctionsInvalidMessage: String = "Invalid function symbol! Use only letters and characters, one forward slash and a" +
    " single digit representing" +
    " the arity!!"
  val FunctionsRegex: Regex = "^[0-9a-zA-Z]+\\/[1-9]$".r

  def clearScreen(): Unit =  print("\033[2J")
}
