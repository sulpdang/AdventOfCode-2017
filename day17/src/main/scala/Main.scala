//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._

object Main extends Day(17) {
  import scala.collection.mutable.ListBuffer

  type Input = Int

  def processedInput = input.head.toInt

  def solve (input:Input) = {
    def solveAcc(curPos:Int, list:ListBuffer[Int], length:Int):Int = {
      length match {
        case 2017 => list(curPos+1)
        case l => 
          val nextPos = (input+curPos) % (length+1) + 1
          list.insert(nextPos, length+1)
          solveAcc(nextPos, list, length+1)
      }
    }
    solveAcc(0, ListBuffer(0), 0)
  }

  def solve2(input:Input) = {
    def solve2Acc(curPos:Int, zeroPos:Int, zeroNextValue:Int, length:Int):Int = {
      length match {
        case 50000000 => zeroNextValue
        case l => {
          ((curPos+input) % length + 1) match {
            case newP if newP == zeroPos+1 => 
              solve2Acc(newP, zeroPos, length, length+1)
            case newP if newP <= zeroPos   => 
              solve2Acc(newP, zeroPos+1, zeroNextValue, length+1)
            case newP => solve2Acc(newP, zeroPos, zeroNextValue, length+1)
          }
        }
      }
    }
    solve2Acc(0, 0, 0, 1)
  }

}
