//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._

case class Instruction(cmd:String, x:String, y:String)

object InstructionUtil {
  def apply(line:String) : Instruction = {
    val Array(cmd, x, y) = line.split(' ').map{_.trim}
    Instruction(cmd, x, y)
  }

}

object Main extends Day(23) {
  type Input = List[Instruction]
  type Memory = Array[Int]


  def processedInput = input.map{InstructionUtil(_)}
  def getValueWithMem(y:String)(mem:Memory) = {
    if(y.forall{_.isLetter}) mem(y.head-'a') else y.toInt
  }

  def solve(input: Input) = {
    val mem = Array.fill('h' - 'a' + 1)(0)
    var cnt = 0
    def getValue(y:String) = getValueWithMem(y)(mem)

    Iterator.iterate{(0, mem)}{case (curPos, mem) =>
      input(curPos) match {
        case Instruction("set", x, y) => mem(x.head - 'a') = getValue(y); (curPos+1, mem)
        case Instruction("sub", x, y) => mem(x.head - 'a') -= getValue(y); (curPos+1, mem)
        case Instruction("mul", x, y) =>
          mem(x.head - 'a') *= getValue(y); cnt+= 1; (curPos+1, mem)
        case Instruction("jnz", x, y) =>
          if(getValue(x) == 0) (curPos+1, mem) else (curPos+getValue(y), mem)
      }
    }.takeWhile{ case (curPos, _) => 0 <= curPos && curPos < input.length }
      .reduce{ (_, b) => b }
    cnt
  }
  def solve2(input: Input) = {
    import math._

    val b = input.head.y.toInt
    val start = b * input(4).y.toInt - input(5).y.toInt
    val end = start - input(7).y.toInt
    def isPrime(x:Int) = (2 to (sqrt(x).toInt + 1)).forall{ x % _ != 0 }
    (start to end by 17).par.filter{!isPrime(_)}.toList.size
  }
}
