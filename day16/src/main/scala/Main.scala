//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//
import myutil._

trait Command extends (Array[Int] => Array[Int])
trait SwapPos extends Command {
  def swap(arr:Array[Int], aPos:Int, bPos:Int) = {
    val temp  = arr(aPos)
    arr(aPos) = arr(bPos)
    arr(bPos) = temp
  }
}

case class Spin(pos:Int) extends Command {
  def apply(arr:Array[Int]) = {
    val (head, tail) = arr.splitAt(arr.length - pos)
    tail++head
  }
}

case class Exchange(aPos:Int, bPos:Int) extends SwapPos {
  def apply(arr:Array[Int]) = {
    swap(arr, aPos, bPos)
    arr
  }
}

case class Partner(aVal:Int, bVal:Int)  extends SwapPos {
  def apply(arr:Array[Int]) = {
    swap(arr, arr.indexOf(aVal), arr.indexOf(bVal))
    arr
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

  def processedInput = input.head.split(',').toList.map{ Command(_) }

  def start = (0 to 15).toArray

  def arrToStr(arr:Array[Int]) = arr.map{_ + 'a'}.map{_.toChar}.mkString("")
  def dance(input:Input, arr:Array[Int]) = input.foldLeft(arr){case (acc, move)=> move(acc)}

  def solve(input:Input)  = arrToStr(dance(input, start))
  def solve2(input:Input) = {

    val startStr = arrToStr(start)

    def getTimes(arr:Array[Int], list:List[String] = List(startStr), times:Int = 0):(List[String], Int) = {
      (dance(input, arr), times) match {
        case (_, 1000000000) => (List(), -1)
        case (x, _) if arrToStr(x) == startStr => (list :+ arrToStr(x), times)
        case (x, _) => getTimes(x, list :+ arrToStr(x), times+1)
      }
    }

    val (list, repeat) = getTimes(start)
    list(1000000000 % repeat)

  }

  val inputFileName2 = "input2.txt"
  val res = getResource(inputFileName2).map{_.split('\n').toList} recoverWith {
    case e:Throwable => {
      val content = readSession.flatMap{ downloadInputContent(_) }
      content.foreach{ x=> writeResource(inputFileName2, x.mkString("\n")) }
      content
    }
  }

}
