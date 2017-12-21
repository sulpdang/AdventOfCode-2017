//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._

case class Pos(x:Int, y:Int) {
  def +(other:Pos) = Pos(x+other.x, y+other.y)
  def *(value:Int) = Pos(x*value, y*value)
  def back =  this * -1
}

object Main extends Day(19) {
  import scala.collection.mutable._

  type RawInput = Array[Array[Char]]
  type Input = ListBuffer[Char]

  def follow(curPos:Pos, input:RawInput, prevPos:Pos = Pos(1, 0), buf:ListBuffer[Pos] = ListBuffer(), set:Set[(Pos, Pos)] = Set() ):ListBuffer[Pos]= {
    curPos match {
      case Pos(x, y) if isValid(curPos, input) && input(curPos.x)(curPos.y) == ' ' => buf
      case Pos(x, y) if input(x)(y) == '+' =>
        val newPos = collectOtherPos(curPos, input).filterNot{_ == prevPos.back}.head
        follow(curPos + newPos, input, newPos, buf :+ curPos, set + ((curPos, prevPos)))
      case Pos(x, y) =>
        follow(curPos + prevPos, input, prevPos, buf :+ curPos, set + ((curPos, prevPos)))
    }
  }

  private def isValid(curPos:Pos, input:RawInput) = {
    val xmax = input.length
    val ymax = input.map(_.length).max - 1
    0 <= curPos.x && curPos.x < xmax && 0 <= curPos.y && curPos.y < ymax
  }

  def collectOtherPos(curPos:Pos, input:RawInput) = {
    val res = List(Pos(1,0), Pos(-1,0), Pos(0,1), Pos(0,-1))
      .filter{ p =>
        val nextPos = p + curPos
        isValid(nextPos, input) && input(nextPos.x)(nextPos.y) != ' '
      }
    res
  }

  def firstLoc(input:RawInput) = input.head.zipWithIndex
    .find{case (a, i) => a == '|'}
    .map{case (a, i) => Pos(0, i)}.get

  lazy val arr = input.map{_.toCharArray}.toArray
  lazy val buf = follow(firstLoc(arr), arr).map{case Pos(x,y) => input(x)(y)}

  def processedInput = buf

  def solve(input:Input)  = buf.filter{Character.isAlphabetic(_)}.mkString("")
  def solve2(input:Input) = buf.size


}
