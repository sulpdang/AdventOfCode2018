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
    val List(a, b) = Iterator(input.sorted).flatten.sliding(2)
      .find{case List(a, b) => (a.diff(b)).size == 1}
      .get.toList
    a.intersect(b)
  }
}
