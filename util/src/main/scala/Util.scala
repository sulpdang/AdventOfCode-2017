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
  type Res1
  type Res2
  
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
    Try{solve(processedInput)} match {
      case Success(x) => println(s"A: ${x.toString}")
      case Failure(e) => println(s"A: Failed(${e.getMessage()} , $e)")
    }

    Try{solve2(processedInput)} match {
      case Success(x) => println(s"B: ${x.toString}")
      case Failure(e) => println(s"B: Failed(${e.getMessage()}, $e)")
    }

  }

  printRes
}
