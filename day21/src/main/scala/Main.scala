//
// Main
//
// Copyright (c) 2019 Yohho (sulpdang@gmail.com)
//
package solver

import myutil._

object Main extends Day(21) {
  import Instruction._
  type Input = Iterator[(Int, Int)]
  def processedInput = {
    def handle(value:Int) = ((value & 16777215) * 65899) & 16777215
    val init = 1765573
    Iterator.iterate((handle(init), 65536)){
      case (value, acc) if acc < 256 => 
        val nextAcc = value | 65536
        val nextValue = handle(init + nextAcc % 256)
        (nextValue, nextAcc)
      case (value, acc) => 
        val nextAcc = acc / 256
        val nextValue = handle(value + nextAcc % 256)
        (nextValue, nextAcc)
    }
  }
  def solve(input:Input) = input.find(_._2 < 256).head._1
  def solve2(input:Input) = {
    val empty = Option.empty[(Int, Int)]
    Iterator.iterate((empty, Set[(Int, Int)](), empty)) {
      case (prev, visited, res) =>
        val curVal = input.next()
        (Some(curVal), prev.map{visited + _}.getOrElse(visited), if(curVal._2 < 256) Some(curVal) else res)
    }.drop(1).takeWhile{case (cur, visited, _) => !visited.contains(cur.get)}
    .map{_._3}.toList.dropRight(1).last.get._1
  }
}
