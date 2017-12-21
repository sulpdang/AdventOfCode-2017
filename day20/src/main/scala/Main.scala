//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._

case class Vector(x:BigInt, y:BigInt, z:BigInt) {
  def +(other:Vector)   = Vector(x + other.x, y + other.y, z + other.z)
  def -(other:Vector)   = Vector(x - other.x, y - other.y, z - other.z)
  def |-|(other:Vector) = {
      (x - other.x) * (x - other.x) +
      (y - other.y) * (y - other.y) +
      (z - other.z) * (z - other.z)
  }
}

object VectorUtil{
  def apply(line:String):Vector = {
    val Array(x,y,z) = line.split(',').map{_.trim}.map{BigInt(_)}
    Vector(x,y,z)
  }
}

case class Particle(id:Int, p:Vector, v:Vector, a:Vector) {
  def move = {
    val newV = v + a
    val newP = p + newV
    Particle(id, newP, newV, a)
  }
}

object Main extends Day(20) {

  type Input = List[Particle]

  def processedInput = {
    val parReg = """p=<([^>]+)>, v=<([^>]+)>, a=<([^>]+)>""".r.unanchored
    input.zipWithIndex.map{case (line,id) =>
      line match {
        case parReg(p, v, a) => { Particle(id, VectorUtil(p), VectorUtil(v), VectorUtil(a)) }
      }
    }
  }
  def solve(input:Input) = input.minBy{x=>Vector(0,0,0) |-| x.a}.id
  def solve2(input:Input) = {
    val iter = Iterator.iterate(input){ particles =>
      particles.map{_.move}.groupBy(_.p)
      .filter{_._2.size == 1}
      .flatMap{_._2}
      .toList
    }
    iter.drop(1000).next.size
  }

}
