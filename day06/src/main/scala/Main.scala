//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

object Main extends App {

  import myutil.Util._
  val memory = readContent.head.split('\t').map{_.toInt}.toList

  def solvePart(memory:List[Int], step:Int = 0, set:Set[List[Int]] = Set()):(Int,List[Int]) = {
    memory match {
      case x if set contains x => (step, x)
      case x =>
        def distribute(memory:List[Int], value:Int, startPos:Int) = {
          val size = memory.size
          val result = memory.updated(startPos, 0)
          val dist = value / size
          val remain = value % size
          val distributesList = (0 until size).map{i=> dist + {if(i < remain) 1 else 0} }
          val (head, tail) = distributesList.splitAt(size - (startPos+1))
          (tail ++ head).zip(result).map{case (r,v) => r + v}.toList
        }
        val max = memory.max
        val firstIdx = memory.indexOf(max)
        val newMemory = distribute(memory, max, firstIdx)
        solvePart(newMemory, step + 1, set + memory)
    }
  }
  def solve = solvePart(memory)._1
  def solve2 = {
    val infLoop = solvePart(memory)._2
    solvePart(infLoop)._1
  }
  println(solve)
  println(solve2)
}
