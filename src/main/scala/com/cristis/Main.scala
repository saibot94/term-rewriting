package com.cristis

import com.cristis.Constants._
import com.cristis.TermRewritingSystem.TRS
import com.cristis.completion.SimpleCompletion
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
    println(s"Please insert an ordering for the function symbols defined above [${functions.mkString(",")}] \nExample: *,i,1. This will mean * > i > 1")
    println("************\n")
    print(">> ")
    Console.flush
    val ordering = StdIn.readLine().trim.split(",").map(_.trim).toList

    println("\n")
    clearScreen()
    println(s"The following constants have been entered: ${constants.mkString(" | ")}")
    println(s"The following function symbols have been entered: ${functions.mkString(" | ")}")

    while (true) {
      println("Please insert a file path containing the initial system E for it to be solved")
      println("************\n")
      print(">> ")
      Console.flush
      val fileName = StdIn.readLine().trim
      val language = new Language(constants, functionsMapped)
      val initialTRS: TRS = language.parseEquationsFile(fileName)
      println("\n")
      println("******* INITIAL TRS: ")
      initialTRS.foreach(println)
      println("\n")
      println("******* Solving with simple completion: ")
      ordering.permutations.foreach { ord =>
        println(s"Trying order: $ord")
        val completionObj = new SimpleCompletion(ord, initialTRS)
        try {
          println(completionObj.solve)
          println(s"System solvable for $ord")
        } catch {
          case e: Exception =>
            println(s"Error solving system for $ord: [ $e ]")
        }
      }
    }
  }
}
