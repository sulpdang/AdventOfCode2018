//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import scala.collection._
import scala.annotation._
import mutable.{Map=>MM}
import myutil._

object Main extends Day(11) {
  type Grouped = (Pos, Pos, Pos)

  type TArray = Array[Array[BigInt]]
  type Input = TArray

  def processedInput = {
    val arr = Array.ofDim[Int](300, 300)
    val serial = input.map{_.toInt}.head
    for{
      x <- (1 to 300).toArray
      y <- (1 to 300).toArray
    } {
      val rackID = x+10
      arr(x-1)(y-1) = (((rackID)*y+serial)*rackID %1000) / 100 - 5
    }
    val subSumArrays = Array.ofDim[BigInt](300, 300)
    subSumArrays(0)(0) = arr(0)(0)
    for(i <- (1 to 299)) {
      subSumArrays(0)(i) = subSumArrays(0)(i-1) + arr(0)(i)
      subSumArrays(i)(0) = subSumArrays(i-1)(0) + arr(i)(0)
    }
    for{
      y <- (1 until 300)
      x <- (1 until 300)
    } {
      subSumArrays(x)(y) = subSumArrays(x)(y-1) + subSumArrays(x-1)(y) - 
                          subSumArrays(x-1)(y-1) + arr(x)(y)
    }
    subSumArrays
  }


  def solvePart(k: Int, subSumArrays:TArray) = {
    def value(x:Int)(y:Int):BigInt = 
      if(x < 0 || y < 0 || x >= 300 || y >= 300) 0 else subSumArrays(x)(y)
    def calAt(x:Int, y:Int) = 
      value(x+k-1)(y+k-1) - value(x-1)(y+k-1) - value(x+k-1)(y-1) + value(x-1)(y-1)

    (for{
      x <- (0 to 300-k).toIterator
      y <- (0 to 300-k)
    } yield Pos(x, y))
      .map{ case pos@Pos(x, y) => (pos, calAt(x, y)) }
      .maxBy{_._2}
  }

  def solve(posValueMap:Input)  = solvePart(3, posValueMap)
  def solve2(posValueMap:Input) =
    (1 to 300).par.map{ size => (solvePart(size, posValueMap), size) }.maxBy{_._1._2}

}
