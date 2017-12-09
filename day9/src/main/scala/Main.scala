//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//


object Main extends App {
  import myutil.Util._ 
  def processIgnored(msg:String) = "!.".r.replaceAllIn(msg, "")
  def processGarbage(msg:String) = "<([^>])+>".r.unanchored.replaceAllIn(msg, "<>")
  def countGroup(msg:String) = {
    def countGroupAcc(remain:String, opened:Int = 0, ans:Int = 0) : Int = {
      remain.headOption match {
        case None => ans
        case Some('{') => countGroupAcc(remain.tail, opened+1, ans)
        case Some('}') => countGroupAcc(remain.tail, opened-1, ans+opened)
        case a => countGroupAcc(remain.tail, opened, ans)
      }
    }
    countGroupAcc(msg)
  }
  def solve = {
    (processIgnored (_:String)) andThen
    (processGarbage) andThen
    (countGroup)
  }
  def solve2(msg:String) = {
    val non = processIgnored (msg)
    non.length - processGarbage(non).length
  }
  println(solve(input.head))
  println(solve2(input.head))
}
