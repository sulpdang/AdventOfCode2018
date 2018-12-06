//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._
import scala.collection.mutable._
import scala.annotation.tailrec

case class Area(leftTop:Pos, rightBottom:Pos) {
  require(leftTop.x <= rightBottom.x && leftTop.y <= rightBottom.y)
  def points = for{
    x <- leftTop.x to rightBottom.x
    y <- leftTop.y to rightBottom.y
  } yield Pos(x, y)
  def bounds = {
    (leftTop.y until rightBottom.y).map{y=> Pos(leftTop.x, y)} ++
    (leftTop.x until rightBottom.x).map{x=> Pos(x, rightBottom.y)} ++
    (leftTop.y+1 to rightBottom.y).map{y=> Pos(rightBottom.x, y)} ++
    (leftTop.x+1 to rightBottom.x).map{x=> Pos(x, leftTop.y)}
  }
  def extend(amount:Int) = Area(leftTop - Pos(amount, amount), rightBottom + Pos(amount, amount))
  def compute(inputPoints:List[Pos]) = {
    points.map{ p => 
      val minDist = inputPoints.map(p.manhattan).min
      inputPoints.filter(p.manhattan(_) == minDist)
    }.filter{_.size == 1}.map{_.head}
     .groupBy(x=>x).mapValues(_.size).toSeq
  }
}

object Main extends Day(6) {
  type Input = List[Pos]

  lazy val processedInput = input.zipWithIndex.map{ case (line, i) => 
    val res = line.split(",").map{_.trim.takeInt}
    Pos(res(0), res(1))
  }
  lazy val minX = processedInput.minBy(_.x).x
  lazy val minY = processedInput.minBy(_.y).y
  lazy val maxX = processedInput.maxBy(_.x).x
  lazy val maxY = processedInput.maxBy(_.y).y

  def solve(input:Input) = {
    val area = Area(Pos(minX, minY), Pos(maxX, maxY))
    val extendedArea = area.extend(100)
    area.compute(input)
      .intersect(extendedArea.compute(input)).map(_._2).max
  }

  def solve2(input:Input)= {

    def findNotChanged(area:Area):Int = {
      @tailrec
      def findNotChangedAcc(amount:Int, res:Int):Int = {
        require(amount > 0)
        val extendedArea = area.extend(amount)
        val bounds = extendedArea.bounds
        val cal = bounds.count{x=>input.map{_.manhattan(x)}.sum < 10000}
        if(cal != 0) findNotChangedAcc(amount+1, cal + res)
        else res
      }
      val cal = area.points.count{x=>input.map{_.manhattan(x)}.sum < 10000}
      findNotChangedAcc(1, cal)
    }
    val mid = (Pos(minX, minY) + Pos(maxX, maxY)) / 2
    val area = Area(mid, mid+Pos(1,1))
    findNotChanged(area)
  }
}
