//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._
import scala.collection._
import scala.collection.mutable.{HashSet, PriorityQueue=>PQ, Map=>MM}
import scala.language.implicitConversions

object Main extends Day(7) {

  type Edge = (Char, Char)
  type Input = List[Edge]

  def processedInput = input.map{x=>(x(5), x(36))}

  implicit def immuMapToMu[A, B](map:immutable.Map[A, B]) = MM[A, B](map.toSeq: _*)

  def solvePart(input:Input, workers:Int)(timeFunc:Char => Int) = {
    import math._
    val queue:PQ[(Int, Char)]  = PQ.empty(Ordering.by{case (t, c) => (-t, -c)})
    val waiting   = PQ[Char]().reverse
    val degrees   = input.groupBy(_._2).mapValues(_.size)
    val connected = input.groupBy(_._1).mapValues(_.map(_._2))

    def solveAcc(degrees:MM[Char, Int], connected:Map[Char, List[Char]], res:List[(Int, Char)]=List()):List[(Int,Char)]= {
      if(queue.isEmpty) res
      else {
        val (time, first) = queue.dequeue
        if(connected contains first) {
          connected(first).foreach{ t => 
            degrees(t) -= 1
            if(degrees(t) == 0) waiting.enqueue(t)
          }
        }
        degrees -= first
        moveToQueue(workers - queue.size, time)
        solveAcc(degrees, connected, res :+ (time, first) )
      }
    }

    def moveToQueue(num:Int, baseTime:Int) = 
      (0 until min(num, waiting.size)).map{x=>waiting.dequeue}
        .foreach{c=>queue.enqueue((baseTime + timeFunc(c), c))}

    waiting ++= connected.keySet.diff(degrees.keySet)
    moveToQueue(workers, 0)
    solveAcc(degrees, connected)
  }

  def solve(input:Input) = solvePart(input, input.size)(c=> 0).map(_._2).mkString("")
  def solve2(input:Input) = solvePart(input, 5)(c=> c-'A' + 61).last._1
}