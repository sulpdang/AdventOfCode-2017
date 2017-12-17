//
// Util
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//
//

package myutil

object Util {

  def readContent:List[String] = {
    scala.io.Source.fromResource("input.txt")
      .mkString
      .split('\n').toList
  }

  def input = readContent
}
abstract class Day(day:Int) extends App {

  type Input
  
  import scala.util._

  def readContent:List[String] = {
    scala.io.Source.fromResource("input.txt")
      .mkString
      .split('\n').toList
  }

  def input = readContent

  def processedInput:Input

  def solve(input:Input):Any
  def solve2(input:Input):Any

  def printRes {

    println(s"--- Day $day ---")

    def num(e:Throwable) = {
      e.getStackTrace().toArray
        .find{_.getFileName contains "Main.scala" }.map{_.getLineNumber} getOrElse (-1)
    }
    def printSolution(func: => Any, prob:String) = {
      Try{func} match {
        case Success(x) => println(s"$prob: ${Option(x).getOrElse("null")}")
        case Failure(e) => println(s"$prob: Failed[line ${num(e)}](${e.getMessage()} , $e)") 
      }
    }

    printSolution(solve{ processedInput }, "A")
    printSolution(solve2{ processedInput }, "B")

  }

  printRes
}
