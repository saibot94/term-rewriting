package com.cristis.functions

import com.cristis.Constants
import com.cristis.TermRewritingSystem.TRS

import scala.collection.mutable

/**
  * Created by cristian.schuszter on 2017-03-20.
  */
class Language(constants: List[String], functions: List[(String, Int)]) {
  var indexMap: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()

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
          case Some(s) =>
            println(s)
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
      cmd.zipWithIndex.foreach {
        case (chr, index) =>
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

  def build(cmdLine: String): Term = {
    if (!validateInput(cmdLine))
      throw new ValidationException
    functions.find(f => cmdLine.startsWith(f._1)) match {
      case Some((name, arity)) =>
        val withoutNameCmdLine = cmdLine.substring(name.length)
        val trimmedCmdLine = withoutNameCmdLine.substring(1, withoutNameCmdLine.lastIndexOf(')'))
        val functionArguments = getTopLevelArgs(trimmedCmdLine)
        Fct(name, functionArguments.map(build))
      case None =>
        if (constants.contains(cmdLine)) {
          Fct(cmdLine)
        } else {
          val index = if(indexMap.get(cmdLine).isDefined) {
            indexMap(cmdLine)
          } else {
            indexMap.put(cmdLine, 0)
            indexMap(cmdLine)
          }
          Var(cmdLine, index)
        }
    }
  }

  /**
    * Parse the equations file that you want to solve in the current language
    * @param filePath the path to be file that you want to load
    * @return
    */
  def parseEquationsFile(filePath: String): TRS = {
    val fileLines = scala.io.Source.fromFile(filePath).getLines()
    indexMap = mutable.HashMap[String, Int]()
    fileLines.map { l =>
      val relations = buildRelations(l)
      indexMap.foreach {case (str, index) => indexMap.put(str, index+1)}
      relations
    }.toList
  }

  /**
    * Build all pairs of terms which lead to the initial set of equations in E.
    * @param cmdLine the line denoting E, E being one of the equations in the language
    */
  def buildRelations(cmdLine: String): (Term, Term) = {
    val splitTerms = cmdLine.split("=")
    (build(splitTerms.head), build(splitTerms.tail.head))
  }

  class ValidationException extends Exception

}
