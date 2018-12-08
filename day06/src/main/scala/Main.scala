//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._
import scala.collection.mutable._
import scala.annotation.tailrec

case class Area(lt:Pos, rb:Pos) {
    require(lt.x <= rb.x && lt.y <= rb.y)
    def points = (for{
      x <- lt.x to rb.x
      y <- lt.y to rb.y
    } yield Pos(x, y)).toList
    def bounds = {
      (lt.y until rb.y).map{y=> Pos(lt.x, y)} ++
      (lt.x+1 to rb.x) .map{x=> Pos(x, lt.y)} ++
      (lt.y+1 to rb.y) .map{y=> Pos(rb.x, y)} ++
      (lt.x until rb.x).map{x=> Pos(x, rb.y)}
    }.toList
    def extend(amount:Int) = Area(lt - Pos(amount, amount), rb + Pos(amount, amount))
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
  def getMinMax(func:Pos=>Int) = 
    (func(processedInput.minBy(func)),func(processedInput.maxBy(func)))
  lazy val (minX, maxX) = getMinMax(_.x)
  lazy val (minY, maxY) = getMinMax(_.y)
  lazy val area = Area(Pos(minX, minY), Pos(maxX, maxY))

  def solve(input:Input) = {
    area.compute(input)
      .intersect(area.extend(100).compute(input))
      .map(_._2).max
  }

  def solve2(input:Input)= {
    def count(points:List[Pos]) = points.count{x=>input.map{_.manhattan(x)}.sum < 10000}
    def findNotChanged(area:Area):Int = {
        @tailrec
        def findNotChangedAcc(amount:Int, res:Int):Int = {
          count(area.extend(amount).bounds) match {
            case 0   => res
            case cal => findNotChangedAcc(amount+1, cal + res)
          }
        }
        findNotChangedAcc(1, count(area.points))
    }
    findNotChanged(area)
  }
}
