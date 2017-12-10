//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//


object Main extends App {
  import myutil.Util._

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

  def solve = {
    val lengths = input.head.split(',').map{_.toInt}.toList
    val array = (0 to 255).toArray
    roundHash(array, lengths, 0, 0)
    array(0) * array(1)
  }

  def solve2 = {
    val firstInput = input.head
    val lengths = firstInput.toCharArray.map{_.toInt}.toList ++ List(17,31,73,47,23)
    val array = (0 to 255).toArray
    (1 to 64).foldLeft((0, 0)){ case ((curPos, skip),_) =>
      roundHash(array, lengths, curPos, skip)
    }
    array.toList.grouped(16).map{lists => lists.reduce(_^_)}
      .toList.map{"%02x" format _}
      .mkString
  }
  println(solve)
  println(solve2)
}
