//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._

object Main extends Day(25) {
  type State = Char
  type Input = Map[Char, (Boolean => (Boolean, Long, Char))]
  def processedInput = Map[Char, (Boolean => (Boolean, Long, Char))]( 
    'A' -> {x => if(!x) (true, 1, 'B')  else (true, -1, 'E')},
    'B' -> {x => if(!x) (true, 1, 'C')  else (true, 1, 'F')},
    'C' -> {x => if(!x) (true, -1, 'D') else (false, 1, 'B')},
    'D' -> {x => if(!x) (true, 1, 'E')  else (false, -1, 'C')},
    'E' -> {x => if(!x) (true, -1, 'A') else (false, 1, 'D')},
    'F' -> {x => if(!x) (true, 1, 'A')  else (true, 1, 'C')}
  )

  def solve(input:Input) = {
    def solveAcc(curState:Char, iter:Long, curPos:Long=0, mem:Set[Long] = Set()):Long = {
      iter match {
        case 0 => mem.size
        case i => 
          val (value, addLoc, nextState) = input(curState)(mem contains curPos)
          solveAcc(nextState, iter-1, curPos + addLoc, if(value) mem + curPos else mem - curPos)
      }
    }
    solveAcc('A', 12459852)
  }
  def solve2(input:Input) = ???
  

}
