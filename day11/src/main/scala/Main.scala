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

  type TArray = Array[Array[Long]]
  type Input = TArray

  lazy val processedInput = {
    val arr = Array.ofDim[Int](300, 300)
    val serial = input.map{_.toInt}.head
    Area(Pos(1, 1), Pos(300, 300)).points
      .foreach{ case Pos(x, y) =>
        val rackID = x+10
        arr(x-1)(y-1) = (((rackID)*y+serial)*rackID %1000) / 100 - 5
      }

    val subSum = Array.ofDim[Long](300, 300)
    subSum(0)(0) = arr(0)(0)

    (1 to 299).foreach{ i=>
      subSum(0)(i) = subSum(0)(i-1) + arr(0)(i)
      subSum(i)(0) = subSum(i-1)(0) + arr(i)(0)
    }

    Area(Pos(1, 1), Pos(299, 299)).points
      .foreach{ case Pos(x, y) =>
        subSum(x)(y) = subSum(x)(y-1) + subSum(x-1)(y) -
                            subSum(x-1)(y-1) + arr(x)(y)
      }
    subSum
  }


  def solvePart(k: Int, subSumArrays:TArray) = {
    def value(x:Int)(y:Int):Long =
      if(x < 0 || y < 0 || x >= 300 || y >= 300) 0 else subSumArrays(x)(y)
    def calAt(x:Int, y:Int) =
      value(x+k-1)(y+k-1) - value(x-1)(y+k-1) - value(x+k-1)(y-1) + value(x-1)(y-1)

    Area(Pos(0, 0), Pos(300, 300)-Pos(k, k)).points
      .map{case pos@Pos(x, y) =>
        (pos+Pos(1,1), calAt(x, y))
      }.maxBy{_._2}

  }

  def solve(posValueMap:Input)  = solvePart(3, posValueMap)
  def solve2(posValueMap:Input) ={
    val ((pos, _), size) = {(1 to 300).toIterator
      .map{ size => (solvePart(size, posValueMap), size) }
      .maxBy{_._1._2}}
    (pos, size)
  }


}
