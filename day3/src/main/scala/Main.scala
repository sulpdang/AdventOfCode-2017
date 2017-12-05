//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

object Main extends App {

  type Pos = (Int, Int)
  val size = (1000, 1000)
  val startPos = size / 2

  private val direction = Array(
     ( 1 , 0),  //right
     ( 0 , -1), //up
     ( -1, 0),  //left
     ( 0 , 1)   //down
  )

  implicit class PosUtil(val arg:Pos) extends AnyVal {
    def *(value:Int) = (arg._1 * value, arg._2 * value)
    def +(value:Pos) = (arg._1 + value._1, arg._2 + value._2)
    def /(value:Int) = (arg._1 / value, arg._2 / value)
  }

  implicit class ArrUtil[T](val arg:Array[Array[T]]) extends AnyVal {
    def apply(value:Pos) = arg(value._1)(value._2)
    def update(pos:Pos, value:T) = arg(pos._1)(pos._2) = value
  }

  def solvePos(loc:Int)(insert:(Array[Array[Int]], Pos) => Unit):Pos = {
    val arr = Array.ofDim[Int](size._1, size._2)

    arr(startPos) = 1
    def value(startPos:Pos, directIdx:Int):Stream[Pos] =
      startPos #:: {
        val targetIdx = (directIdx + 1) % 4
        val targetPos = startPos + direction(targetIdx)

        val (nextPos, nextIdx) = 
          if(arr(targetPos) == 0) (targetPos, targetIdx) 
          else (startPos + direction(directIdx), directIdx)

        insert(arr, nextPos)
        value(nextPos, nextIdx)

      }

      value(startPos, 1).dropWhile{pos => arr(pos) < loc}.head
  }

  def solve(loc:Int):Int = {
    import math._
    if(loc == 1) 0
    else {
      var cur = 2
      val (x, y) = solvePos(loc){case (arr, pos) =>
        arr(pos) = cur
        cur += 1
      }
      abs(x - startPos._1) + abs(y - startPos._2)
    }
  }

  def solve2(loc:Int):Int = {
    import math._

    val arr = Array.ofDim[Int](size._1, size._2)
    arr(startPos) = 1

    def addCurValue(value:Int, pos:Pos) = {
      def isPossible(value:Int) = 0 <= value && value < size._1
      List(
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1), (0, 1),
        (1, -1), (1, 0), (1, 1)
      ).map{ pos + _ }.filter{case (x, y) => isPossible(x) && isPossible(y)}.foreach{ pos => arr(pos) += value
      }
    }

    addCurValue(1, startPos)

    if(loc == 1) 1
    else {
      val (x, y)= solvePos(loc){ case (answer, pos) =>
        addCurValue(arr(pos), pos)
        answer(pos) = arr(pos)
      }
      arr(x, y)
    }
  }

  /*
  println(solve(1))
  println(solve(2))
  println(solve(23))
  println(solve(806))
  */
  println(solve(10))
  println(solve2(10))
}
