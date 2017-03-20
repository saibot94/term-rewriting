package com.cristis.console

import scala.io.StdIn
import scala.util.matching.Regex

/**
  * Created by cristian.schuszter on 2017-03-20.
  */
class ConsolePrompter(prompt: String,
                      invalidOption: String,
                      validator: Regex) {

  def read(): List[String] = Console.withIn(System.in) {
    var validChoice = false
    var constants = Array[String]()
    var choice: String = null
    do {
      println("\n"+ prompt)
      println("************\n")
      print(">> ")
      Console.flush
      choice = StdIn.readLine().trim
      if(choice != null && choice.nonEmpty) {
        validChoice = validator.findFirstMatchIn(choice) match {
          case Some(f) =>
            constants +:= f.toString
            true
          case None => false
        }
        if (!validChoice) {
          println("\n" + invalidOption)
        }
      }
    } while (choice != null && choice.nonEmpty)
    Console.flush
    constants.toList
  }
}
