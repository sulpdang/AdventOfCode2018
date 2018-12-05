//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

import scala.collection._
import myutil._
import scala.annotation.tailrec

object Main extends Day(2) {
  type Input = List[String]
  def processedInput = input
  def solve(input:Input) = {
    val grouped = input.map{_.groupBy(identity) }.map{_.values.map{_.size}.toList}
    grouped.count{_ contains 2} * grouped.count{_ contains 3}
  }
  def solve2(input:Input) = {
    def traverse(sorted:Input):String = {
      sorted match {
        case a :: b :: tail => 
          val intersect = a.intersect(b)
          if(a.size - 1 == intersect.size) intersect
          else traverse(sorted.tail)
        case x => throw new Exception("Not Found")
      }
    }
    traverse(input.sorted)
  }
}
