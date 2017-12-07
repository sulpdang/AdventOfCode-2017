//
// Util
//
// Copyright (c) 2017 Yohho (sulpdang@gmail.com)
//
//

package myutil

object Util {

  def readContent:List[String] = {
    scala.io.Source.fromResource("input.txt")
      .mkString
      .split('\n').toList
  }

}
