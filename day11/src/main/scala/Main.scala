//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

object Main extends App {
  import myutil.Util._

  val map = Map[String, Pos] ( 
    "ne" -> Pos(-1, 1),
    "n"  -> Pos(-2, 0),
    "nw" -> Pos(-1, -1),
    "se" -> Pos(1, 1),
    "s"  -> Pos(2, 0),
    "sw" -> Pos(1, -1)
  )
  case class Pos(x:Int, y:Int) {
    import math._
    def +(other:Pos) = Pos(x + other.x, y + other.y)
    def dist = (abs(x) + abs(y)) / 2
  }
  def solve(directions:List[String]) = {
    directions.foldLeft(Pos(0, 0)){case (pos, direction) =>
      pos + map(direction)
    }.dist
  }
  def solve2(directions:List[String]) = {
    directions.map{map(_)}.scanLeft(Pos(0, 0)){_ + _}.maxBy(_.dist).dist
  }
  val directions = input.head.split(',').toList
  println(solve(directions))
  println(solve2(directions))

}
