//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._
import scala.collection.mutable.Map

object Main extends Day(9) {
  type Input = (Int, Int)

  case class Node(value:Int) {
    var prev:Node=this
    var next:Node=this
    def |<->|(node:Node) { node.prev -- this; this -- node }
    def --(node:Node) { this.next = node; node.prev = this }
  }

  def processedInput = {
    val Array(player, worth) = input.head.split(';')
    (player.takeInt,worth.drop(22).takeInt)
  }
  def solvePart(numOfPlayer:Int, worth:Int):Long = {
    val playerScores = Map[Int, Long]().withDefaultValue(0)
    val startNode = Node(0)

    def solveAcc(node:Node, player:Int, num:Int):Node = {
      (player, num) match {
        case (_, n) if n == worth => node
        case (p, n) if p > numOfPlayer => solveAcc(node, 1, n)
        case (p, n) if n%23 == 0 => {
          val prev7Node = 7.foldLeft(node){case (acc, _)=>acc.prev}
          val prevPrev7Node = prev7Node.prev
          val prevNext7Node = prev7Node.next
          prevPrev7Node -- prevNext7Node
          playerScores(p) += prev7Node.value + n
          solveAcc(prevNext7Node, p+1, n+1)
        }
        case (p, n) => {
          val newNode = Node(n)
          newNode |<->| node.next.next
          solveAcc(newNode, p+1, n+1)
        }
      }
    }
    solveAcc(startNode.next, 1, 1)
    playerScores.values.max
  }
  def solve(input:Input) = solvePart(input._1, input._2)
  def solve2(input:Input) = solvePart(input._1, input._2*100)
}
