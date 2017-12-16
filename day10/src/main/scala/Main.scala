//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//


import myutil._

object Main extends Day(10) {
  import KnotHash._
  type Input = String

  def processedInput = input.head

  def solve(input:Input) = {
    val lengths = input.split(',').map{_.toInt}.toList
    val array = (0 to 255).toArray
    roundHash(array, lengths, 0, 0)
    array(0) * array(1)
  }

  def solve2(input:Input) = knotHash(input).mkString("")

}

object KnotHash {

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

  def roundHash(arr:Array[Int], lengths:List[Int], curPos:Int, skip:Int) :(Int, Int) = {
      val size = arr.size
      lengths.foldLeft((curPos, skip)) { case ((startPos, skip), length) => {
        reverseArray(arr, startPos, length, size)
        ((startPos + length + skip) % size, skip+1)
      }}
  }

  def knotHash(inputString:String) = {
    val lengths = inputString.toCharArray.map{_.toInt}.toList ++ List(17,31,73,47,23)
    val array = (0 to 255).toArray
    (1 to 64).foldLeft((0, 0)){ case ((curPos, skip),_) =>
      roundHash(array, lengths, curPos, skip)
    }
    array.grouped(16)
      .map{_.reduce(_^_)}
      .map{"%02x" format _}
      .flatMap{_.toCharArray}.toList
  }

}
