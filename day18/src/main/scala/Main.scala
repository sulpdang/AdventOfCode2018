//
// Main
//
// Copyright (c) 2019 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._

object Main extends Day(18) {

  type ArceMap = Map[Pos, Arce]
  type Input   = Stream[ArceMap]


  abstract class Arce(nextArce: => Arce, val v:Char) {
    def change(map:Map[Pos, Arce], pos:Pos):Arce = {
      val adjacentArce = pos.adjacent.map{map.get}.collect{case Some(x) => x}
      if(check(adjacentArce)) nextArce else this
    }
    def check(cond:Seq[Arce]):Boolean
  }
  case object Open extends Arce(Trees, '.') {
    def check(cond:Seq[Arce]):Boolean = cond.count{_ == Trees} >= 3
  }
  case object Trees extends Arce(LumberYard, '|') {
    def check(cond:Seq[Arce]):Boolean = cond.count{_ == LumberYard} >= 3
  }
  case object LumberYard extends Arce(Open, '#') {
    def check(cond:Seq[Arce]):Boolean = !(cond.count{_ == Trees} >= 1 && cond.count{_ == LumberYard} >= 1)
  }
  lazy val (xLen, yLen) = (input(0).length, input.length)

  def processedInput = {
    val madeMap:ArceMap = input.zipWithIndex.flatMap{case (line, y) => 
      line.zipWithIndex.collect{
        case ('.', x) =>  Pos(x, y) -> Open
        case ('|', x) =>  Pos(x, y) -> Trees
        case ('#', x) =>  Pos(x, y) -> LumberYard
      }
    }.toMap
    Iterator.iterate(madeMap){ map => 
      map.map{case (pos, arce) => pos -> arce.change(map, pos) }
    }.toStream
  }
  def resourceValue(map:String) = map.count{_ == '|'} * map.count{_ == '#'}
  def solve(input:Input) = resourceValue(input.take(11).map{mapToStr}.last)
  def solve2(input:Input) = {
    val strMap = input.map{mapToStr}
    val repeated = strMap.scanLeft(Set[String]()){_ + _}.zip(strMap)
      .takeWhile{case (acc, elem) => !(acc.contains(elem))}.toSeq
    val firstSeenIdx = strMap.indexOf(strMap(repeated.size))
    val time = 1000000000L
    val correctIndex = ((time - firstSeenIdx) % (repeated.size - firstSeenIdx) + firstSeenIdx).toInt
    resourceValue(repeated(correctIndex)._2)
  }

  def mapToStr(map:Map[Pos, Arce]):String = {
    (0 until yLen).map{y =>
      (0 until xLen).map{x=> map(Pos(x, y)).v}.mkString("")
    }.mkString("\n")
  }
}
