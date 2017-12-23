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

trait Types {
  import scala.collection.mutable.Map
  type Memory = Map[String, Long]
  type Instruction = (Int,Memory) => Result
}

object Instruction extends Types {

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

object Main extends Day(18) with Types{

  import scala.collection.mutable._

  type Input = List[Instruction]

  def genMemory = (0 to 1).toList.map{ id => Map[String, Long]("p" -> id.toLong).withDefaultValue(0) }

  def genInstructions(input:Input, wiredMap:List[Memory]) =
    wiredMap.map{ mem =>
      Iterator.iterate[(Result, Memory)]((Normal(0), mem)) {
        case (result, mem) => (input(result.pos)(result.pos, mem), mem)
      }.takeWhile{case (x, mem) => x.pos < input.length}
       .collect{case (x:Abnormal, mem) => (x, mem)}
    }

  def processedInput = input.map{Instruction(_)}

  def solve(input:Input)  = genInstructions(input, genMemory)(0)
    .takeWhile{case (Rcv(_, reg), mem) => mem(reg) == 0; case _ => true}
    .collect{case (x:Snd, _) => x}
    .reduce{(_,b)=> b}.value

  def solve2(input:Input) = {
    val mem       = genMemory
    val generator = genInstructions(input, mem)
    val iterators = generator.zipWithIndex.map{ case (gen, id) =>
      if(!gen.hasNext) Iterator.empty
      else Iterator.iterate(gen.next){
        case (Snd(_, value), mem) => Storage(1-id) :+= value; gen.next
        case (x@Rcv(_, reg), mem) =>
          Storage(id) match {
            case head::tail => { mem(reg) = head ; Storage(id) = tail ; gen.next ; }
            case Nil => (x, mem)
          }
        case _ => gen.next
      }
    }
    iterators(0).zip(iterators(1)).map{case (a, b) => (a._1, b._1)}
      .takeWhile{case (a:Rcv, b:Rcv)=> !Storage.isAllEmpty; case _=> true}
      .count{case (_, b:Snd) => true; case _=> false}
  }

}
