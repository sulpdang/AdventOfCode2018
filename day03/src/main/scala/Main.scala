//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import scala.collection.mutable._
import myutil._
import scala.language.implicitConversions

case class Square(id:Int, start:Pos, size:Pos) {
  val end = start + size
  val area = Area(start, end-Pos(1, 1))
  def points = area.points
}

object Main extends Day(3) {
  type Input = List[Square]

  implicit def strToInt(str:String) = str.toInt 

  lazy val pattern = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r.unanchored
  def processedInput = input.map{ 
    case pattern(id, x, y, w, h) => Square(id, Pos(x, y), Pos(w, h))
  }
  lazy val map = {
    val res = Map[Pos, List[Int]]().withDefaultValue(List())
    processedInput.foreach{square => 
      square.points.foreach{res(_) :+= square.id }
    }
    res
  }
  lazy val squareMap = processedInput.map{x => (x.id,x)}.toMap
  def solve(input:Input) = map.count{case (k, v) => v.size >= 2}
  def solve2(input:Input) = {
    map.map{_._2}.filter{_.size <= 1}.flatten
      .groupBy{x=>x}.find{ case(id, value)=>
        val Pos(w, h) = squareMap(id).size
        w*h == value.size
      }.map{_._1}.get
  }
}
