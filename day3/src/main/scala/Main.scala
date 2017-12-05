//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

object Main extends App {

  def findPosition(loc:Int):(Int, Int) = {
    val pos = Array(
      (1,-1),
      (1,1),
      (-1,1),
      (-1,-1),
      (1,-1)
    )
    def findLastPos:(Int, Int) = {
      val size = (math.sqrt(loc)-1).toInt / 2 * 2 + 3
      (size * size, size)
    }
    def findMinMax(max:Int, size:Int, posList:Array[(Int, Int)] = pos):(Int, Int) = {
      loc match {
        case x if max - size < x && x <= max => (max-size, max)
        case x => findMinMax(max-size+1, size, posList.tail)
      }
    }
    val (max, size) = findLastPos
    findMinMax(max, size)
  }

  def solve(loc:Int):Int = {
    if(loc == 1) 0
    else {
      val (x, y) = findPosition(loc)
      x+y
    }
  }
  println(solve(1) == 0)
  println(solve(12) == 3)
  println(solve(23) == 2)
  println(solve(1024) == 31)
}
