//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

import scala.collection._
import myutil._
import scala.annotation.tailrec

object Main extends Day(1) {
  type Input = List[Int]
  def processedInput = input.map{_.toInt}
  def solve(input:Input) = input.sum
  def solve2(input:Input) = {
    @tailrec
    def travel(list:Input=input, set:Set[Int]=Set(), curVal:Int = 0):Int = {
      val nextVal = curVal + list.head
      if(set contains nextVal) nextVal
      else travel(list.tail :+ list.head, set + nextVal, nextVal)
    }
    travel()
  }
}
