//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._

trait Result{ val pos:Int }

case class Normal(val pos:Int) extends Result
trait Abnormal extends Result
case class Snd(val pos:Int, val value:Long) extends Abnormal
case class Rcv(val pos:Int, val reg:String) extends Abnormal

object Types {
  import scala.collection.mutable.Map
  type Memory = Map[String, Long]
  type Instruction = (Int,Memory) => Result
}

object Instruction {
  import Types._

  private def getValue(dontKnow:String, map:Memory) = {
    if(dontKnow.forall{Character.isAlphabetic(_)}) map(dontKnow)
    else dontKnow.toLong
  }

  def apply(line:String):Instruction = {
    val (head, reg, valueOrReg) = line.split(' ') match {
      case Array(a, b)    => (a, b, "")
      case Array(a, b, c) => (a, b, c)
    }
    head match {
      case "set" => (curPos, mem) => mem(reg)  = getValue(valueOrReg, mem) ; Normal(curPos+1)
      case "add" => (curPos, mem) => mem(reg) += getValue(valueOrReg, mem) ; Normal(curPos+1)
      case "mul" => (curPos, mem) => mem(reg) *= getValue(valueOrReg, mem) ; Normal(curPos+1)
      case "mod" => (curPos, mem) => mem(reg) %= getValue(valueOrReg, mem) ; Normal(curPos+1)
      case "snd" => (curPos, mem) => Snd(curPos+1, getValue(reg, mem))
      case "rcv" => (curPos, mem) => Rcv(curPos+1, reg)
      case "jgz" => (curPos, mem) => 
        if(getValue(reg, mem) > 0) Normal(curPos + getValue(valueOrReg, mem).intValue)
        else Normal(curPos+1)
    }
  }

}

object Storage {
  import collection.mutable._
  val queued        = Array.fill(2)(List[Long]())
  def isAllEmpty    = queued.forall(_.isEmpty)
  def apply(id:Int) = queued(id)
  def update(id:Int, list:List[Long]) = queued(id) = list
}

object Main extends Day(18) {

  import Types._
  import scala.collection.mutable._

  type Input = List[Instruction]

  def genMemory = (0 to 1).toList.map{ id => Map[String, Long]("p" -> id.toLong).withDefaultValue(0) }

  def genInstructions(input:Input, wiredMap:List[Memory]) =
    wiredMap.map{ mem =>
      Iterator.iterate[(Result, Memory)]((Normal(0), mem)) {
        case (x, mem) => (input(x.pos)(x.pos, mem), mem)
      }.takeWhile{case (x, mem) => x.pos < input.length}
       .collect{case (x:Abnormal, mem) => (x, mem)}
    }

  def processedInput = input.map{Instruction(_)}

  def solve(input:Input)  = genInstructions(input, genMemory)(0)
    .takeWhile{!_._1.isInstanceOf[Rcv]}
    .collect{case (x:Snd, _) => x}
    .reduce{(_,b)=> b}.value

  def solve2(input:Input) = {
    val mem = genMemory
    val generator = genInstructions(input, mem)
    val iterators = (0 to 1).toList.zip(generator).map{ case (id, gen) =>
      if(!gen.hasNext) Iterator.empty

      else Iterator.iterate(gen.next){ case (prev, mem) =>

        (Storage(id), prev) match {
          case (_ , Snd(_, value))  =>
            { Storage(1-id) :+= value; gen.next }
          case (head::tail, Rcv(_, reg)) =>
            { mem(reg) = head ; Storage(id) = tail ; gen.next ; }
          case (Nil, x:Rcv) => (x, mem)
          case _    => gen.next
        }

      }
    }
    Iterator.iterate[(Result, Result)]((Normal(0), Normal(0))) { case (prevA, prevB) =>
      (iterators(0).next._1, iterators(1).next._1)
    }.takeWhile{case (a:Rcv, b:Rcv)=> !Storage.isAllEmpty; case _=> true}
     .count{case (_, b:Snd) => true; case _=> false}
  }

}
