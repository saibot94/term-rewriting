package com.cristis

import com.cristis.Constants._
import com.cristis.console.ConsolePrompter
import com.cristis.functions.Language

import scala.io.StdIn

object Main {

  def main(args: Array[String]): Unit = {
    mainLoop()
  }


  def mainLoop(): Unit = {
    // Constants phase
    val constantsPrompt = new ConsolePrompter(ConstantsPrompt, ConstantsInvalidMessage, ConstantsRegex)
    val constants: List[String] = constantsPrompt.read().distinct
    println("\n\n")
    // Functions phase
    val functionsPrompt = new ConsolePrompter(FunctionsPrompt, FunctionsInvalidMessage, FunctionsRegex)
    val functions: List[String] = functionsPrompt.read().distinct
    val functionsMapped: List[(String, Int)] = functions.map(s => {
      val spl = s.split("/")
      (spl(0), spl(1).toInt)
    })

    println("\n")
    clearScreen()
    println(s"The following constants have been entered: ${constants.mkString(" | ")}")
    println(s"The following function symbols have been entered: ${functions.mkString(" | ")}")
    println("Enter a valid expression in the language that you've defined above." +
      "\nExample: f(g(1,2), 0)\n******")
    while(true) {
      print(">> ")
      Console.flush
      val language = new Language(constants, functionsMapped)
      val input = StdIn.readLine().replaceAll(" ", "")
      if(language.validateInput(input)) {
        println("The expression was valid!")
      } else {
        println("ERROR: Could not validate the expression!")
      }
      println("\n")
    }
  }
}
