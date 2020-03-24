//
// solver.Main
//
// Copyright (c) 2019 Yohho (sulpdang@gmail.com)
//
package solver

import myutil._
import collection.mutable.{Map => MM}
import scala.collection.mutable

object Main extends Day(22) {
  type Input = (Pos, Dynamic[Pos, Int])
  lazy val processedInput = {
    val Array(x, y) = input(1).drop(8).split(',').map{_.toInt}
    val (depth, targetPos) = (input(0).drop(7).takeInt, Pos(x, y))
    val div = 20183
    val xTimes = 16807
    val yTimes = 48271 % div
    @inline def makeAns(value:Int) = ((value + depth) % div)
    val func = { (p:Pos, prev:Dynamic[Pos, Int]) =>
      p match {
        case `targetPos` | Pos(0, 0) => makeAns(0)
        case Pos(0, y) => makeAns(y*yTimes)
        case Pos(x, 0) => makeAns(x*xTimes)
        case Pos(x, y) => makeAns(prev(Pos(x-1, y)) * prev(Pos(x, y-1)))
      }
    }
    (targetPos, new Dynamic[Pos, Int](func).mapValue{_ % 3})
  }
  def solve(input:Input) = {
    val (target, map) = input
    Area(Pos(0, 0), target).points.map{map(_)}.sum
  }

  trait Tool
  case object Torch extends Tool
  case object ClimbingGear extends Tool
  case object Neither extends Tool
  case class State(pos:Pos, tool:Tool) {
    def next = for{
      near <- pos.nears
      tool <- Seq(Neither, Torch, ClimbingGear)
      if near.x >= 0 && near.y >= 0
    } yield State(near, tool)
  }

  case class StateIncludeCost(state:State, cost:Int=0, prev:Option[StateIncludeCost] = None) extends Ordered[StateIncludeCost] {
    def next(prohibitTool:Dynamic[Pos, Tool], memo:MM[State, Int]):List[StateIncludeCost] = {
      val res = state.next.filter{case state => state.tool != prohibitTool(state.pos)}
        .filter{case nextState => nextState.tool != prohibitTool(state.pos)}
        .map{ case nextState@State(pos, tool)=>
          StateIncludeCost(nextState, cost + (if(tool == state.tool) 1 else 8), Some(this))
        }.filter{case state => !memo.contains(state.state) || state.cost < memo(state.state) }
      res.foreach{case stateIncCost => memo(stateIncCost.state) = stateIncCost.cost }
      res
    }
    def compare(that:StateIncludeCost) = that.cost - this.cost
  }

  def solve2(input:Input) = {
    val (target, map) = input
    val prohibitTool = Seq(Neither, Torch, ClimbingGear)
    val prohibitToolMap:Dynamic[Pos, Tool] = map.mapValue{prohibitTool(_)}
    val startState = State(Pos(0, 0), Torch)
    val priorityQueue = mutable.PriorityQueue[StateIncludeCost](StateIncludeCost(startState))
    Iterator.iterate((priorityQueue, MM(startState -> 0))) { case (queue, mmm) =>
        val minElem = queue.dequeue
        (queue ++ minElem.next(prohibitToolMap, mmm), mmm)
    }.dropWhile{ case (q, _) => q.head.state != State(target, Torch) }.next._1.head.cost
  }
}
