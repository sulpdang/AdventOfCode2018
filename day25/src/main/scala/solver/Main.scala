//
// solver.Main
//
// Copyright (c) 2020 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._

object Main extends Day(25) {

  type Input = List[Pos4D]

  case class Pos4D(a:Int, b:Int, c:Int, d:Int) {
    import math._
    def |-|(other:Pos4D) = {
      (a - other.a).abs + (b - other.b).abs + (c - other.c).abs + (d - other.d).abs
    }
  }

  def processedInput = {
    input.map{_.split(',').toList.map{_.toInt}}.map{ case List(a, b, c, d) => 
      Pos4D(a, b, c, d)
    }
  }

  def solve(input:Input) = {
    import collection.mutable.{Map => MM}
    val root = MM(input.map{x => x -> x}:_*)
    def find(a:Pos4D):Pos4D = {
      if(root(a) == a) a 
      else {
        root(a) = find(root(a))
        root(a)
      }
    }
    def union(a:Pos4D, b:Pos4D) {
      val x = find(a)
      val y = find(b)
      if(x != y) { root(y) = x }
    }
    for{
      a <- input
      b <- input
      if a != b && (a |-| b) <= 3
    } { union(a, b) }
    root.values.toSet.map(find(_)).size

  }
  def solve2(input:Input) = ???

}