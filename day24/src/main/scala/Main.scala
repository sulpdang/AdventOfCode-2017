//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._
import scala.collection._
import myutil._

case class Component(a:Int, b:Int) {
  def contains(c:Int) = a == c || b == c
  def getNext(c:Int) = if(a == c) b else a
}

case class Result(l:Int, m:Int)

object Main extends Day(24) {
  type Input  = List[Component]

  def processedInput = input
    .map{_.split('/').map{_.toInt}.sorted}
    .map{case Array(a,b)=> Component(a.toInt, b.toInt)}


  def traverseChild(id:Int, remains:Set[Component], max:Int=0, length:Int = 0)
  (implicit order:Ordering[Result]):Result = {
    if(remains.isEmpty) Result(length, max)
    else {
      val connected = remains.filter{_ contains id}
      if(connected.isEmpty) Result(length, max)
      else {
        connected.map{ x =>
          val nextID = x.getNext(id)
          traverseChild(nextID, remains - x, max + nextID + id, length+1)
        }.max
      }
    }
  }

  def solve(input:Input) = {
   implicit def order:Ordering[Result] = Ordering.by(e=>e.m)
    traverseChild(0, input.toSet)
  }

  def solve2(input:Input) = {
   implicit def order:Ordering[Result] = Ordering.by(e=>(e.l, e.m))
   traverseChild(0, input.toSet)
  }


}
