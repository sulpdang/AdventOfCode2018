//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

import scala.collection.mutable._
import myutil._

case class Square(id:Int, start:Pos, size:Pos) {
  val end = start + size
}

object Main extends Day(3) {
  type Input = List[Square]

  def processedInput = input.map{ line => 
    val Array(id, posAndSize) = line.split('@')
    val Array(pos, size) = posAndSize.split(':')
    val Array(x, y) = pos.trim.split(',').map{_.toInt}
    val Array(w, h) = size.trim.split('x').map{_.toInt}
    Square(id.trim.tail.toInt, Pos(x, y), Pos(w, h))
  }
  lazy val map = {
    val res = Map[(Int, Int), ListBuffer[Int]]()
    processedInput.foreach{square=> 
      for{
        x <- square.start.x until square.end.x
        y <- square.start.y until square.end.y
      } {
        if(!res.contains((x,y))) res((x,y)) = ListBuffer()
        res((x, y)) += square.id
      }
    }
    res
  }
  lazy val squareMap = {
    processedInput.map{ square => (square.id , square) }.toMap
  }
  def solve(input:Input) = {
    map.count{case (k, v) => v.size >= 2}
  }
  def solve2(input:Input) = {
    map.map{_._2}.filter{_.size <= 1}.flatten.groupBy{x=>x}.find{case(id, value)=>
      val square = squareMap(id)
      val Pos(w, h) = square.size
      w*h == value.size
    }.map{_._1}
  }
}
