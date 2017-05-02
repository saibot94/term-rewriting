package com.cristis.functions

import com.cristis.Constants

/**
  * Created by cristian.schuszter on 2017-03-20.
  */
class Language(constants: List[String], functions: List[(String, Int)]) {

  def validateInput(cmdLine: String): Boolean = {
    functions.find(f => cmdLine.startsWith(f._1)) match {
      case Some((name, arity)) =>
        val withoutNameCmdLine = cmdLine.substring(name.length)
        if (withoutNameCmdLine.startsWith("(") && withoutNameCmdLine.endsWith(")")) {
          val trimmedCmdLine = withoutNameCmdLine.substring(1, withoutNameCmdLine.lastIndexOf(')'))
          val functionArguments = getTopLevelArgs(trimmedCmdLine)
          if (functionArguments.length != arity) {
            false
          } else {
            functionArguments.forall(arg => validateInput(arg))
          }
        } else {
          false
        }
      case None =>
        val trimmed = cmdLine.trim
        Constants.AllExceptLettersAndNumbersRegex.findFirstMatchIn(trimmed) match {
          case Some(s) => println(s)
            false
          case None =>
            val splitCmdLine = trimmed.split(",")
            if (splitCmdLine.nonEmpty && splitCmdLine.forall(s => constants.contains(s) || LangUtils.validVariableSymbol(s))) {
              true
            } else {
              false
            }
        }
    }
  }

  private def getTopLevelArgs(cmd: String): List[String] = {
    var level = 0
    var previousCommaIndex = 0
    if (cmd.indexOf(',') == -1) {
      List(cmd)
    } else {
      var res: List[String] = Nil
      cmd.zipWithIndex.foreach { case (chr, index) =>
        if (chr == '(') {
          level += 1
        } else if (chr == ')') {
          level -= 1
        } else if (level == 0 && chr == ',') {
          res = res :+ cmd.substring(if (previousCommaIndex == 0) previousCommaIndex else previousCommaIndex + 1, index)
          previousCommaIndex = index
        }
      }
      res :+ cmd.substring(if (previousCommaIndex == 0) previousCommaIndex else previousCommaIndex + 1)
    }
  }
}
