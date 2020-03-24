//
// Main
//
// Copyright (c) 2019 Yohho (sulpdang@gmail.com)
//
package solver

import myutil._
import scala.collection.mutable
import scala.annotation.tailrec

object Main extends Day(20) {
  import collection._
  import collection.mutable.{PriorityQueue => PQ, Map => MM}

  type Room  = Node[Pos]
  type Input = Iterator[Seq[Room]]

  def bfs[T](start:T)(nextFunc:T => Seq[T]):Iterator[Seq[T]] = 
    Iterator.iterate((Seq(start), Set[T](start))){case (cur, visited) => 
      val next = cur.flatMap{nextFunc}.filterNot{visited.contains}.distinct
      (next, visited ++ next)
    }.map{_._1}.takeWhile{!_.isEmpty}
 
  @tailrec
  private def makeDoors(remain:String, prev:Set[Room], child:Set[Room]=Set(),
  map:MM[Pos, Room]=MM().withDefault(Node(_)), state:List[(Set[Room], Set[Room])]=Nil):Unit = {
    val direction:Map[Char,(Pos=>Pos)] = 
      Map('E' -> {_.right}, 'W' -> {_.left}, 'N' -> {_.up}, 'S' -> {_.down})
    remain.head match {
      case '$' => 
      case '(' => makeDoors(remain.tail, prev, Set(), map, (prev, child) +: state)
      case '|' => makeDoors(remain.tail, state.head._1, child ++ prev, map, state)
      case ')' => makeDoors(remain.tail, state.head._1 ++ child ++ prev, state.head._2, map, state.tail)
      case x   => 
        val cur = for(room <- prev) yield {
          val nextRoom = map(direction(x)(room.value))
          nextRoom -- room
          nextRoom
        }
        makeDoors(remain.tail, cur, child, map, state)
    }
  }

  def processedInput:Input = {
    val init = Node(Pos(0, 0))
    makeDoors(input.head.tail, prev=Set(init))
    bfs(init){_.neighbors}
  }
  def solve(input:Input)  = input.size - 1
  def solve2(input:Input) = input.drop(1000).foldLeft(0){_ + _.size}
 
}