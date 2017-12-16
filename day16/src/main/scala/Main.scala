//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//
import myutil._

trait Command{
  def apply(origin:Array[Int]):Array[Int]
}

case class Spin(pos:Int) extends Command {
  def apply(arr:Array[Int]) = {
    val (head, tail) = arr.splitAt(arr.length - pos)
    tail++head
  }
}
case class Exchange(aPos:Int, bPos:Int) extends Command {
  def apply(arr:Array[Int]) = {
    val temp = arr(aPos)
    arr(aPos) = arr(bPos)
    arr(bPos) = temp
    arr
  }
}

case class Partner(aVal:Int, bVal:Int)  extends Command {
  def apply(arr:Array[Int]) = {
    val aInd = arr.indexOf(aVal)
    val bInd = arr.indexOf(bVal)
    val temp = arr(aInd)
    arr(aInd) = arr(bInd)
    arr(bInd) = temp
    arr
  }
  override def equals(other:Any) = {
    other match {
      case o:Partner =>
      (o.aVal == aVal && o.bVal == bVal) || (o.aVal == bVal) && (o.bVal == aVal)
      case _ => false
    }
  }
}

object Command {
  def apply(str:String) = {
    str.head match {
      case 's' => Spin(str.tail.toInt)
      case 'x' => {
        val Array(a, b) = str.tail.split('/')
        Exchange(a.toInt, b.toInt)
      }
      case 'p' => {
        val Array(a, b) = str.tail.split('/').map{_.head - 'a'}
        Partner(a, b)
      }
    }
  }
}

object Main extends Day(16) {
  type Input = List[Command]

  def start = (0 to 15).toArray

  def arrToStr(arr:Array[Int]) = arr.map{_ + 'a'}.map{_.toChar}.mkString("")

  def processedInput = input.head.split(',').toList.map{ Command(_) }

  def oneRound(input:Input, arr:Array[Int]) = input.foldLeft(arr){case (acc, elem)=> elem(acc)}

  def solve(input:Input)  = { arrToStr(oneRound(input, start)) }
  def solve2(input:Input) =  { 
    val solvedTwoMore = oneRound(input.collect{
      case x : Exchange => x
      case x : Spin => x
    }, start)
    val solvedOne = oneRound(input, start)
    val table = (1 to 32).scanLeft(solvedTwoMore){case (acc, elem) =>
      acc.map{i=>acc(i)}
    }.toArray
    table(0) = solvedOne
    val res = 1000000000.toBinaryString.reverse
      .zipWithIndex.collect{case (a, i) if a == '1' => i}
      .toArray
      .foldLeft(start) { case (acc, elem) => table(elem).map{i=>acc(i)} }
    arrToStr(res)
  }

}
