//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._

object Types {
  import scala.collection.mutable._
  type Rule = Map[BaseMatrix, BaseMatrix]
}

case class BaseMatrix(values:Array[String]) {
  import Types._
  private def transpose = {
    require(values.length == 2 || values.length == 3, values.mkString("\n"))
    BaseMatrix(values.head.indices.map{i => values.map{_(i)}.mkString("")}.toArray)
  }

  def flipVert = BaseMatrix(values.map{_.reverse})
  def flipHori = BaseMatrix(values.reverse)

  def rotate90  = transpose.flipVert
  def rotate180 = rotate90.rotate90
  def rotate_90 = flipVert.transpose

  def similar = List(
    rotate_90, rotate90, rotate180,
    flipVert, flipVert.rotate90, flipVert.rotate180, flipVert.rotate_90
  )

  def next(rule:Rule) = {
    if(rule contains this) { rule(this) }
    else {
      val cond = similar.find{rule contains _} match {
        case Some(x) => x
        case None => throw new Exception(values.mkString("\n"))
      }
      val result = rule(cond)
      rule(this) = result
      result
    }
  }

  override def equals(other:Any):Boolean = {
    other match {
      case o:BaseMatrix => values.zip(o.values).forall{case (a, b) => a == b}
      case _ => false
    }
  }
  override def toString = values.mkString("\n")
  override def hashCode = toString.hashCode
}

case class Matrix(val values:Array[String]) extends AnyVal {
  import Types._

  def nextMatrix(rule:Rule):Matrix = {
    val length = values.length
    def extract(i:Int, j:Int, amount:Int) =
      BaseMatrix(values.slice(i, i+amount).map{_.slice(j, j + amount)})

    def insertToTarget(target:Array[Array[Char]], nextBaseMatrix:BaseMatrix, px:Int, py:Int) {
      val values = nextBaseMatrix.values
      for{
        i <- (0 until values.size)
        j <- (0 until values.size)
      } {
        target(i+px)(j+py) = values(i)(j)
      }
    }

    def nextWithInterval(interval:Int, nextInterval:Int) =  {
        val nextLength = length / interval * nextInterval
        val target = Array.ofDim[Char](nextLength, nextLength)
        for{
          (i, ni) <- (0 until (length) by interval).zip(0 until nextLength by nextInterval)
          (j, nj) <- (0 until (length) by interval).zip(0 until nextLength by nextInterval)
        } {
          val baseMatrix = extract(i, j, interval)
          val next = baseMatrix.next(rule)
          insertToTarget(target, next, ni, nj)
        }
        Matrix(target.map{_.mkString("")})
    }
    length match {
      case l if l % 2 == 0 => nextWithInterval(2, 3)
      case l =>
        require(length % 3 == 0)
        nextWithInterval(3, 4)
    }
  }

  override def toString = values.map{_.mkString("")}.mkString("\n")

  def countBit = values.map{_.count{_ == '#'}}.sum
}

object Rule {
  def apply(line:String) = {
    val Array(cond, res) = line.split("=>").map{_.trim.split('/').toArray}
    BaseMatrix(cond) -> BaseMatrix(res)
  }
}

object Main extends Day(21) {

  import collection.mutable._
  import Types._

  type Input = Array[String]

  def start = """.#.
  |..#
  |###""".stripMargin.split('\n').map(_.trim).toArray

  def processedInput = input.toArray

  lazy val rule = Map(input.map{Rule(_)}: _*)

  def solve(input:Input) = {
    memory.clear
    (1 to 5).foldLeft{Matrix(start)} {case (a, _) => a.nextMatrix(rule)}.countBit
  }

  def solve2(input:Input) = {
    memory.clear
    (1 to 18).foldLeft{Matrix(start)}{case (a, _) => a.nextMatrix(rule)}.countBit
  }
}

