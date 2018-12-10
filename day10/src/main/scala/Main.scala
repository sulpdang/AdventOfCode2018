//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._
import scala.language.implicitConversions


case class Point(var loc:Pos, val velocity:Pos) {
  def y = loc.y
  def move = Point(loc + velocity,velocity)
}

object Main extends Day(10) {

  type Input = List[(Int, List[Point])]
  lazy val r = "position=<([- 0-9]+),([- 0-9]+)> velocity=<([- 0-9]+),([- 0-9]+)>".r

  implicit def strToInt(str:String) = str.trim.toInt
  def processedInput = {
    val points = input.map{
      case r(a,b,c,d) => Point(Pos(a,b), Pos(c,d))
    }
    Iterator.iterate((0, points)){case (second, input) => (second+1, input.map{_.move})}
      .take(100000).filter{case(i, points) =>
      val yMapped = points.map{_.y}
      yMapped.max - yMapped.min < 10
    }.toList
  }

  def draw(list:List[Point]) = {
    val locMapped = list.map{_.loc}
    val area = Area.minMaxArea(locMapped)
    area.extend(3).points.groupBy(_.y).toList.sortBy(_._1).map(_._2).map{ t => 
      t.map{
        case x if locMapped contains x => '#'
        case x => '.'
      }
    }.foreach{println}
  }

  def solve(input:Input) = {
    input.foreach{case (_, points)=> draw(points)}
  }
  def solve2(input:Input) = {
    input.map{case (i, _) => i}
  }
}
