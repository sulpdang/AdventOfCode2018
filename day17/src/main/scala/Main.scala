//
// Main
//
// Copyright (c) 2019 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._

object Main extends Day(17) {
  type Input = MyMap

  type World = Map[Pos, Tile]
  type Poses = List[Pos]

  class Tile(ch:Char) {
    override def toString = ch.toString
  }

  abstract class Water(val ch:Char) extends Tile(ch)
  abstract class BaseWall(val ch:Char) extends Tile(ch)
  case object Wall          extends BaseWall('#')
  case object VerticalFlow  extends Water('|')
  case object HorizonalFlow extends Water('~')

  implicit class PosesUtil(val pos:Poses) extends AnyVal {
    def toVertical  = pos.map{_ -> VerticalFlow}.toMap
    def toHorizonal = pos.map{_ -> HorizonalFlow}.toMap
  }

  case class MyMap(maps:World, minY:Int, maxY:Int, flowPosSeq:Poses = List()) {
    def isWall(p:Pos) = maps.get(p) == Some(Wall)
    def flow(cond:Pos => Boolean)(posFunc:Pos => Pos)(pos:Pos):Poses = {
      lazy val stream:Stream[Pos] = posFunc(pos) #:: stream.map{posFunc}
      stream.takeWhile{cond}.toList
    }
    def verticalFlow  = flow{p => !maps.contains(p) && p.y <= maxY}{_.down} _
    def horizonalFlow = flow{p => 
      maps.contains(p.down) && maps(p.down) != VerticalFlow && !isWall(p) 
    } _

    def flow:MyMap = {
      flowPosSeq match {
        case Nil => this
        case head::tail => { (verticalFlow)
            .andThen{ p => (p, if(p.isEmpty) head else p.last) }
            .andThen{ case (poses, target) =>
              maps.get(target.down) match {
                case Some(VerticalFlow) | None => (poses.toVertical, poses)
                case _ => {
                  val lefts = horizonalFlow{_.left}(target)
                  val rights = horizonalFlow{_.right}(target)
                  val edge = List((target +: lefts).last.left, (target +: rights).last.right).filterNot{isWall}
                  val nextFlowPosSeq = edge ++ (poses.reverse :+ head).diff(lefts ++ rights :+ target) 
                  val flowNeedCheck = (lefts ++ rights ++ edge :+ target)
                  val nextMaps = if(edge.isEmpty) flowNeedCheck.toHorizonal else flowNeedCheck.toVertical
                  (poses.toVertical ++ nextMaps, nextFlowPosSeq)
                }
              }
            }.andThen{case (nextMaps, nextFlow) => 
              this.copy(maps = maps ++ nextMaps, flowPosSeq = nextFlow ++ tail)
            }(head)
        }
      }
    }
    def water = maps.collect{case (k, v:Water) if k.y >= minY && k.y <= maxY => v}
    def isEnd = flowPosSeq.length == 0
  }

  lazy val rY = """x=(\d+), y=(\d+)..(\d+)""".r
  lazy val rX = """y=(\d+), x=(\d+)..(\d+)""".r

  def processedInput = {
    val walls = input.flatMap {
      case rY(x, y1, y2) => (y1.toInt to y2.toInt).map{y => Pos(x.toInt,y)}
      case rX(y, x1, x2) => (x1.toInt to x2.toInt).map{x => Pos(x,y.toInt)}
    }
    val Area(Pos(minX, minY), Pos(maxX, maxY)) = Area.minMaxArea(walls)
    val start = Pos(500, minY)
    val map = MyMap(walls.map{_ -> Wall}.toMap + (start -> VerticalFlow), minY = minY, maxY = maxY, flowPosSeq = List(start))
    lazy val solve:Stream[MyMap] = map #:: solve.map{_.flow}
    solve.takeWhile(!_.isEnd).last
  }

  def solve(lastMap:Input) = lastMap.water.size
  def solve2(lastMap:Input) = lastMap.water.count{_ == HorizonalFlow}

  private def mapToStr(map:World):String = {
    val Area(Pos(minX, minY), Pos(maxX, maxY)) = Area.minMaxArea(map.keys.toList)
    (minY to maxY).map{ y =>
      (minX to maxX).map{x => Pos(x, y)}.map{pos => map.get(pos) match {
        case Some(x) => x.toString()
        case _ => '.'
      }}.mkString("")
    }.mkString("\n")
  }
}
