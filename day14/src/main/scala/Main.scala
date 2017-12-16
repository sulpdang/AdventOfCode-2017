//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._
import scala.language.implicitConversions

object Main extends Day(14) {

  import KnotHash._

  type Input = String
  case class Pos(x:Int, y:Int) {
    def +(other:Pos) = Pos(x + other.x, y + other.y)
  }

  implicit class IntUtil(val ivalue:Int) extends AnyVal {
    def toBinaryPadString(num:Int, padStr:String) = {
      (padStr * num + ivalue.toBinaryString).takeRight(num)
    }
  }

  def processedInput = input.head

  def makeMap(input:Input) = {
    val hexString = "0123456789abcdef"
    for(i <- (0 to 127).toList) yield {
      (for{
        c   <- knotHash(s"${input}-$i")
        bit <- hexString.indexOf(c).toBinaryPadString(4,"0")
      } yield bit).mkString("")
    }
  }

  def solve(input:Input) = makeMap(input).map{_.count{_ == '1'}}.sum
  def solve2(input:Input) = {
    val map = makeMap(input).map{_.toArray}
    val direction = List((1,0), (-1,0), (0,1), (0,-1))
    def dfs(x:Int, y:Int) {
      def isPossible(x:Int) = 0 <= x && x <= 127
      direction
        .map    {case (i,j) => (i+x, j+y)}
        .filter {case (i,j) => isPossible(i) && isPossible(j)}
        .filter {case (i,j) => map(i)(j) == '1'}
        .foreach{case (i,j) => map(i)(j) = '0'; dfs(i, j) }
    }
    (for{
      i <- (0 until 128)
      j <- (0 until 128)
    } yield i -> j)
      .iterator
      .foldLeft(0){ 
        case (acc, (i, j)) if map(i)(j) == '1' => dfs(i, j); acc + 1
        case (acc, _) => acc
      }
  }
}


