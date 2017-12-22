//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

object Main extends App {
  import myutil.Util._
  type Word = String
  type Words = Array[Word]
  def readContent:Array[Words] = {
    scala.io.Source.fromResource("input.txt")
      .mkString
      .split('\n')
      .map{_.split(' ').toArray}
  }
  def solve(wordsContainer:Array[Words]):Int = {
    wordsContainer.count{words => 
      words.toSet.size == words.size
    }
  }
  def solve2(wordsContainer:Array[Words]):Int = {
    wordsContainer.count { words => 
      words.map{_.sorted}.toSet.size == words.size
    }
  }
  println(solve(readContent))
  println(solve2(readContent))
}
