//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._
import scala.collection._
import scala.collection.mutable.{HashSet, PriorityQueue}
import scala.language.implicitConversions

object Main extends Day(7) {

  type Input = (PriorityQueue[Node], MM, MM)

  type Node = Char
  type Edge  = (Node, Node)
  type MM = mutable.Map[Node, HashSet[Node]]
  type Time = Int
  type TimeInfo = (Time, Node)
  type Schedule = PriorityQueue[TimeInfo]
  type TaskList = PriorityQueue[Node]

  implicit class MapUtil(val map:MM) extends AnyVal {
    def removeOppMapAndGetNextTasks(opp:MM, n:Node) =
      map(n).filter{ p => opp(p) -= n; opp(p).size == 0 }
  }

  implicit class NodeUtil(val node:Node) extends AnyVal {
    def toTimeInfo(curTime:Time)(implicit timeInfo:Time) = 
      (curTime + (node - 'A')+timeInfo + 1, node)
  }

  implicit def immuMapToMu[A, B](map:immutable.Map[A, B]) = mutable.Map[A, B](map.toSeq: _*)

  def processedInput = {
    val newInput = input.map{ x => (x(5), x(36)) }
    def toDistinct(func:Edge=>Node) = newInput.map(func).distinct
    val parents   = toDistinct(_._1)
    val children  = toDistinct(_._2)
    val start     = PriorityQueue(parents.diff(children):_*).reverse
    val direct    = newInput.groupBy{_._1}.mapValues(x=> HashSet(x.map(_._2):_*))
    val opposite  = newInput.groupBy{_._2}.mapValues(x=> HashSet(x.map(_._1):_*))
    (start, direct, opposite)
  }

  def solve(input:Input) = {
    val (queue, direct, opposite) = input
    def solveAcc(current:PriorityQueue[Node], right:MM, opp:MM, res:List[Node]=List()):String = {
      if(current.isEmpty) res.mkString("")
      else {
          val h = current.dequeue()
          val nextVisit = 
            if(right.contains(h)) right.removeOppMapAndGetNextTasks(opp, h) 
            else HashSet()
          solveAcc((current ++ nextVisit), right, opp, res :+ h)
      }
    }
    solveAcc(queue, direct, opposite)
  }

  def solve2(input:Input) = {
    val workers = 5
    implicit val addTime:Time = 60
    implicit val ordering:Ordering[TimeInfo] = Ordering.by{case (t, _) => -t}

    val (queue, direct, opposite) = input

    def solve2Acc(curTime:Time = 0, working:Schedule, remain:TaskList, right:MM, opp:MM):Time = {
        def getNextWorker(time:Time, queue:Schedule) =  {
          val (nextTasks, nextRemain) = remain.splitAt(workers - working.size)
          solve2Acc(time, queue ++ nextTasks.map{_.toTimeInfo(time)}, nextRemain, right, opp)
        }
        working match {
          case queue if queue.isEmpty && remain.isEmpty => curTime
          case queue if queue.isEmpty => getNextWorker(curTime, queue)
          case queue => {
            val (t, n) = queue.dequeue()
            if(right.contains(n)) 
              remain ++= right.removeOppMapAndGetNextTasks(opp, n)
            getNextWorker(t, queue)
          }
        }
    }
    solve2Acc(0, PriorityQueue(), queue, direct, opposite)
  }
}
