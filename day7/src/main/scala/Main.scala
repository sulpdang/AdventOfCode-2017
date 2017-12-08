//
// Main
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//

case class Node(name:String, weight:Int, child:List[String])

object Nodes {
  private val Bottom    = """(\w+) \((\d+)\)""".r
  private val NotBottom = """(\w+) \((\d+)\) -> (.+)$""".r
  def apply(line:String):Node = {
    line match {
      case Bottom(name, weightStr) =>
        Node(name, weightStr.toInt, List())
      case NotBottom(name, weightStr, childLine) =>
        Node(name, weightStr.toInt, childLine.split(',').map{_.trim}.toList)
      case _ => throw new Error(line)
    }
  }
}

trait TreeElem {
  type T <: TreeElem
  val name:String
  val weight:Int
  val totWeight:Int
  val isBalanced:Boolean
  def wrongToRightWeight:Option[Int]
}
case class Leaf(name:String, weight:Int) extends TreeElem {
  lazy val totWeight:Int = weight
  val isBalanced:Boolean = true
  def wrongToRightWeight:Option[Int] = None
}

case class Elem(name:String, weight:Int, child:List[TreeElem]) extends TreeElem {
  lazy val totWeight:Int = weight + child.map{_.totWeight}.sum
  lazy val isBalanced:Boolean = {
    child match {
      case head::Nil => head.isBalanced
      case x => 
        val t = x.map{_.totWeight}
        t.min == t.max
    }
  }

  def wrongToRightWeight:Option[Int] = {
    if(child.forall{_.isBalanced}) {
      val (abnormal, normal) = child.groupBy{_.totWeight}.map{case (_, v)=> v}
        .partition{_.size == 1}
      val abnormalHead = abnormal.head.head
      val normalHead = normal.head.head
      Some(abnormalHead.weight + (normalHead.totWeight - abnormalHead.totWeight))
    } else child.find{!_.isBalanced}.flatMap{_.wrongToRightWeight}
  }
}

object Tree {
  def apply(nodes:List[Node]) = {
    import collection.mutable.Map
    val map = Map[String, TreeElem]()
    def findChild(remain:List[Node]):TreeElem = {
      remain match {
        case Node(name, w, child)::Nil => Elem(name, w, child.map{map(_)})
        case x => {
          val (hasChild, noChild) = remain
            .partition{node => node.child.forall{map contains _}}
          hasChild.foreach{ case Node(name, w, childs) =>
            map(name) = Elem(name, w, childs.map{map(_)}) 
          }
          findChild(noChild)
        }
      }
    }
    val (leaves, elems) = nodes.partition{x=> x.child.size == 0}
    leaves.foreach{case Node(name, weight, _)=> map(name) = Leaf(name, weight)}
    findChild(elems)
  }
}

object Main extends App {
  import myutil.Util._
  val root = Tree(input.map{Nodes(_)})
  def solve = root
  def solve2 = root.wrongToRightWeight
  println(solve.name)
  println(solve2)
}
