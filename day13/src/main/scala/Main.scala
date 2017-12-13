//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//


case class FireWall(val id:Int, depth:Int) {

  private val period = (depth - 1) * 2
  private def lastPos(curPos:Int) = {
    val newStart = (curPos+id) % period
    next().take(newStart+1).last
  }
  def getValue(curPos:Int = 0) = if(lastPos(curPos) == 0) id * depth else 0
  def isCaught(curPos:Int = 0) = lastPos(curPos) == 0

  private def next(curPos:Int = 0, direction:Int = 1):Stream[Int] = curPos #:: {
    if(curPos == depth-1) next(curPos-1, -1)
    else if(curPos == 0) next(curPos+1, 1)
    else next(curPos+direction, direction)
  }

}

object Main extends App {
  import myutil.Util._
  import scala.collection._

  val processIn = input.map{line=>line.split(":").toList.map{_.trim.toInt}}.map{case List(a,b) => FireWall(a, b)}

  def solve = processIn.map{_.getValue()}.sum
  def solve2 = Stream.from(0).find{i=> processIn.forall{!_.isCaught(i)} }.get
  println(solve)
  println(solve2)
}
