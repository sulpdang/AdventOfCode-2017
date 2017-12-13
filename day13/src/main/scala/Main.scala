//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//


case class FireWall(val id:Int, depth:Int) {

  private val period = (depth - 1) * 2
  def lastPos(curPos:Int) = (curPos+id) % period
  def getValue(curPos:Int = 0) = if(lastPos(curPos) == 0) id * depth else 0
  def isCaught(curPos:Int = 0) = lastPos(curPos) == 0

}

object Main extends App {

  import myutil.Util._
  import scala.collection._

  val processIn = input.map{line=>line.split(":").toList.map{_.trim.toInt}}.map{case List(a,b) => FireWall(a, b)}

  def solve = processIn.map{_.getValue()}.sum
  def solve2 = Stream.from(0).filter{i=> processIn.forall{!_.isCaught(i)}}.head
  println(solve)
  println(solve2)

}
