//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

object Main extends App {
  import myutil.Util._
  val arr = readContent.map{_.toInt}.toArray
  def solvePart(function: Int => Int, step:Int = 0, curIdx:Int = 0):Int =  {
    curIdx match {
      case x if x >= arr.size || x < 0 => step
      case x => {
        val nextIds = curIdx + arr(curIdx)
        arr(curIdx) = function(arr(curIdx))
        solvePart(function, step+1, nextIds)
      }
    }
  }
  def solve  = solvePart{x => x+1}
  def solve2 = solvePart{x => if(x>=3) x-1 else x+1}
  println(solve2)
}
