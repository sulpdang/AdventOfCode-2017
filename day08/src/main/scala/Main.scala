//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

object Main extends App {
  import myutil.Util._
  import collection.mutable.Map

  type Cond = (String, Int) => Boolean
  type Command = (String, Int) => Unit

  val storage = Map[String, Int]().withDefaultValue(0)
  val condition    = Map[String, Cond]( 
    ">" -> {(reg, v)  => storage(reg) > v } ,
    "<" -> {(reg, v)  => storage(reg) < v } ,
    "==" -> {(reg, v) => storage(reg) == v } ,
    "!=" -> {(reg, v) => storage(reg) != v } ,
    "<=" -> {(reg, v) => storage(reg) <= v } ,
    ">=" -> {(reg, v) => storage(reg) >= v }
  )
  val command = Map[String, Command] (
    "inc" -> {(reg, v) => storage(reg) += v},
    "dec" -> {(reg, v) => storage(reg) -= v},
  )
  val processInput = input.map{
    x=>x.split("if").map{_.trim.split("\\s+").toList}.toList
  }
  def solve = {
      processInput.foreach{
        case List(regA, com, valueA) :: List(regB, cond, valueB) :: Nil =>  {
          if(!(storage contains regA)) storage(regA) = 0
          if(condition(cond)(regB, valueB.toInt)) {
            command(com)(regA, valueA.toInt)
          }
        }
        case x => throw new Exception(s"Not Recognized $x")
      }
      val res = storage.toList.map{_._2}.max
      storage.clear
      res
  }
  def solve2 = {
      var max = 0 
      processInput.foreach{
        case List(regA, com, valueA) :: List(regB, cond, valueB) :: Nil =>  {
          if(!(storage contains regA)) storage(regA) = 0
          if(condition(cond)(regB, valueB.toInt)) {
            command(com)(regA, valueA.toInt)
            if(storage(regA) > max) max = storage(regA)
          }
        }
        case x => throw new Exception(s"Not Recognized $x")
      }
      max
  }
  println(solve)
  println(solve2)
}
