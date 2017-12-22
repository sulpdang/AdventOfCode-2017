//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

import myutil._

object Direction {

  private val direction = Array(
    Vector(1, 0), Vector(0, 1), Vector(-1, 0), Vector(0, -1)
  )
  def apply(dirIdx:Int) = direction(dirIdx % 4)

}

trait Status

case object Weakened extends Status
case object Infected extends Status
case object Flagged extends Status
case object Clean extends Status

case class Vector(x:Int, y:Int) {
  def +(other:Vector) = Vector(x + other.x, y + other.y)
}

case class Virus(pos:Vector, dir:Int) {
  def move(stats:Map[Vector, Status])(func: (Status, Int) => (Status, Int))
    :(Map[Vector, Status], Virus) = {
    val (newStatus, newDir) = func(stats(pos), dir)
    (stats + (pos -> newStatus) , Virus(pos + Direction(newDir), newDir % 4))
  }
}


object Main extends Day(22) {

  type Input = Map[Vector, Status]

  def processedInput = {
    val length = input.length
    val mid    = length / 2
    val infected = for{
      i <- (0 until length)
      j <- (0 until length)
      if input(i)(j) == '#'
    } yield (Vector(j - mid, i - mid) -> Infected)
    infected.foldLeft(Map[Vector,Status]().withDefaultValue(Clean)){_ + _}
  }

  lazy val virus = Virus(Vector(0, 0), 3)

  def solve(input:Input) = {
    var cnt = 0
    val res = (1 to 10000).foldLeft{(input, virus)}{ case ((status, virus),_ ) =>
      virus.move(status) { case (status, dir) =>
        status match {
          case Infected => (Clean, dir + 1)
          case Clean    => cnt += 1;(Infected, dir + 3)
        }
      }
    }
    cnt
  }
  def solve2(input:Input) = {

    var cnt = 0
    val res = (1 to 10000000).foldLeft{(input, virus)}{ case ((status, virus),_ ) =>
      virus.move(status) { case (status, dir) =>
        status match {
          case Weakened => cnt += 1;(Infected, dir)
          case Infected => (Flagged, dir + 1)
          case Flagged  => (Clean, dir + 2)
          case Clean    => (Weakened, dir + 3)
        }
      }
    }
    cnt
  }

}
