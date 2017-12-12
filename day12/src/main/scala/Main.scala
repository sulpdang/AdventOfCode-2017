//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

class Union_FindSet(max:Int) {
  private val _parent = (0 to max).toArray
  def parent = _parent.clone
  def find(u:Int):Int = {
    if(u != _parent(u)) _parent(u) = find(_parent(u))
    _parent(u)
  }
  def union(u:Int, v:Int) = {
    val up = find(u)
    val vp = find(v)
    if(up != vp) _parent(up) = vp
  }
  def connected(u:Int, v:Int):Boolean = find(u) == find(v)
}

object Main extends App {
  import myutil.Util._

  val processedIn = input.map{line=>
    val Array(u, v) = line.split("<->")
    val iu = u.trim.toInt
    val ivList = v.split(",").map{_.trim.toInt}.toList
    (iu, ivList)
  }

  val g = new Union_FindSet(1999)
  for{
    (u, vList) <- processedIn
    v <- vList
  } {
    g.union(u, v)
  }

  def solve = (0 to 1999).filter{g.connected(0, _)}.size
  def solve2 = { g.parent.groupBy(identity).size }
  println(solve)
  println(solve2)
}
