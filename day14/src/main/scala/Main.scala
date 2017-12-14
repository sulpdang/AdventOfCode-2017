//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._

object Main extends Day(13) {

  import KnotHash._

  type Input = String

  def processedInput = input.head

  def makeMap(input:Input) = {
    val hexString = "0123456789abcdef"
    (0 to 127)
      .map{i => knotHash(s"${input}-$i")
        .flatMap{ hexString.indexOf(_).toBinaryString.reverse.padTo(4,"0").reverse }
        .mkString("")
        .toCharArray
      }.toArray
  }

  def solve(input:Input)  = makeMap(input).map{_.count{_ == '1'}}.sum

  def solve2(input:Input) = {
    val map = makeMap(input)
    val direction = List((1,0), (-1,0), (0,1), (0,-1))
    def dfs(x:Int, y:Int) {
      def isPossible(x:Int) = 0 <= x && x <= 127
      map(x)(y) match {
        case '1' =>  {
          map(x)(y) = '0'
          direction
            .filter {case (a,b) => isPossible(a+x) && isPossible(b+y)}
            .foreach{case (a,b) => dfs(a+x, b+y) }
        }
        case _ =>
      }
    }

    var t = 0
    for{
      i <- (0 until 128)
      j <- (0 until 128)
      if map(i)(j) == '1'
    } {
      dfs(i, j)
      t += 1
    }
    t
  }

}


object KnotHash {

  def reverseArray(arr:Array[Int], s:Int, l:Int, size:Int) = {
    for{
      i <- (0 until l)
      end = ((l - 1 - i + s) % size)
      curPos = (i + s) % size
      if curPos <= end
    } {
      val temp = arr(end)
      arr(end) = arr(curPos) ; arr(curPos) = temp
    }
  }

  def roundHash( arr:Array[Int], lengths:List[Int], curPos:Int, skip:Int) :(Int, Int) = {
      val size = arr.size
      lengths.foldLeft((curPos, skip)) { case ((startPos, skip), length) => {
        reverseArray(arr, startPos, length, size)
        ((startPos + length + skip) % size, skip+1)
      }}
  }

  def knotHash(inputString:String) = {
    val lengths = inputString.toCharArray.map{_.toInt}.toList ++ List(17,31,73,47,23)
    val array = (0 to 255).toArray
    (1 to 64).foldLeft((0, 0)){ case ((curPos, skip),_) =>
      roundHash(array, lengths, curPos, skip)
    }
    array.grouped(16)
      .map{_.reduce(_^_)}
      .map{"%02x" format _}
      .flatMap{_.toCharArray}.toList
  }

}
