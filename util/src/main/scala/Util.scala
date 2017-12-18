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

object SimpleHtml {

  import java.net._
  import util._

  def getInputFromSite(urlStr:String, session:String) = {
    val url = new URL(urlStr)
    val con = url.openConnection().asInstanceOf[HttpURLConnection]
    con.setRequestMethod("GET")
    con.setRequestProperty("Cookie", s"session=$session")
    con.setUseCaches(false)
    con.connect

    con.getResponseCode match {
      case 200 =>
        Success(scala.io.Source.fromInputStream(con.getInputStream).getLines.toList)
      case x   => Failure(new Exception(s"Not appropriate response code $x"))
    }
  }

}

abstract class Day(day:Int) extends App {

  type Input

  import scala.util._

  def sessionFile:String = "../util/.session"

  def readContent:List[String] = {
    scala.io.Source.fromResource("input.txt").mkString.split('\n').toList
  }

  val readSession = { Try(scala.io.Source.fromFile(sessionFile).mkString("").trim) }
  val inputFileName = "input.txt"

  def getResource(filename:String) =
    Try{scala.io.Source.fromResource(filename).mkString("")}

  def downloadInputContent(session:String):Try[List[String]] = 
    SimpleHtml.getInputFromSite(s"http://adventofcode.com/2017/day/$day/input", session)

  def writeResource(fileName:String, content:String):Try[Unit] = {
    Try{
      import java.io._
      val file = new File(s"./src/main/resources/$fileName")
      println(file.getAbsolutePath)
      val pw = new PrintWriter(file)
      pw.write(content)
      pw.close()
    }
  }

  def readInput:Try[List[String]] = {

    import java.net.URL

    getResource(inputFileName).map{_.split('\n').toList} recoverWith {
      case e:Throwable => {
        val content = readSession.flatMap{ downloadInputContent(_) }
        println("Hi")
        content.foreach{ x=> writeResource(inputFileName, x.mkString("\n")) }
        content
      }
    }

  }

  def input = readInput match {
    case Success(e) => e
    case Failure(e) => throw e
  }

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
        case Failure(e) => e.printStackTrace;println(s"$prob: Failed[line ${num(e)}](${e.getMessage()} , $e)")
      }
    }

    printSolution(solve{ processedInput }, "A")
    printSolution(solve2{ processedInput }, "B")

  }

  printRes
}
