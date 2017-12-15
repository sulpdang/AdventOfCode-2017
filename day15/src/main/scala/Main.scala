//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//


import myutil._

object Main extends Day(15) {

  import scala.annotation._

  type Input = List[BigInt]

  implicit class BigIntUtil(val cur:BigInt) extends AnyVal {
    def nextValue(usedNum:Int)(func:BigInt => Boolean) = {
      def mod = 2147483647

      @tailrec
      def nextValueAcc(value:BigInt):BigInt = {
        val nextValue = (value * usedNum) % mod
        if(func(nextValue)) nextValue
        else nextValueAcc(nextValue)
      }
      nextValueAcc(cur)
    }
  }

  def processedInput:Input = input.map{BigInt(_)}

  def countMatches(funcA: BigInt => Boolean, funcB:BigInt=>Boolean, max:Int) = {

    val List(initA, initB) = processedInput
    val lowest = BigInt((1 << 16) - 1)

    @tailrec 
    def countMatchesAcc(a:BigInt, b:BigInt, acc:Int = 0, cur:Int = 1):Int = {
      if(cur == max) acc
      else {
        val nextA = a.nextValue(16807)(funcA)
        val nextB = b.nextValue(48271)(funcB)
        val nextAcc = if((a & lowest).intValue == (b & lowest).intValue) acc + 1 else acc
        countMatchesAcc(nextA, nextB, nextAcc, cur+1)
      }
    }
    countMatchesAcc(initA, initB)
  }

  def solve(input:Input) = countMatches(a=>true, b=>true, 40000000)
  def solve2(input:Input) = countMatches(a=> a % 4 == 0, b => b % 8 == 0, 5000000)

}
