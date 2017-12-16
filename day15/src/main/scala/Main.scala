//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//


import myutil._

object Main extends Day(15) {

  import scala.annotation._

  type Input = List[BigInt]

  class Generator(n:BigInt, num:BigInt) extends Iterator[BigInt] {
    private var value = n
    private def mod = 2147483647
    def hasNext = true
    def next() = {
      value = value * num % mod
      value
    }
  }

  def processedInput:Input = input.map{BigInt(_)}

  def countMatches(funcA: BigInt => Boolean, funcB:BigInt=>Boolean, max:Int) = {

    val List(initA, initB) = processedInput
    val lowest = BigInt((1 << 16) - 1)

    val aGen = new Generator(initA, 16807).filter{funcA}.map{_ & lowest}
    val bGen = new Generator(initB, 48271).filter{funcB}.map{_ & lowest}

    aGen.zip(bGen).take(max).count{case (a,b) => a.intValue == b.intValue}

  }

  def solve(input:Input)  = countMatches(a=>true, b=>true, 40000000)
  def solve2(input:Input) = countMatches((_ & 3) == 0, (_ & 7) == 0, 5000000)

}
