package com.cristis.functions

import com.cristis.Constants

/**
  * Created by darkg on 3/28/2017.
  */
object LangUtils {

  import Constants.ValidVariableRegex

  def validVariableSymbol(s: String): Boolean = ValidVariableRegex.findFirstIn(s) match {
    case Some(_) => true
    case None => false
  }

}
