//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import scala.collection._
import scala.annotation._
import myutil._

object Main extends Day(11) {
  type Grouped = (Pos, Pos, Pos)

  type TArray = Dynamic[Pos, Int]
  type Input = TArray

  lazy val processedInput = {
    val arr = Array.ofDim[Int](300, 300)
    val serial = input.head.takeInt
    def value(x:Int, y:Int) = {
      val rackID = (x+1)+10
      (((rackID)*(y+1)+serial)*rackID %1000) / 100 - 5
    }
    new Dynamic[Pos, Int]({
      case (Pos(0, 0), map) => value(0, 0)
      case (Pos(0, y), map) => map(Pos(0,y-1)) + value(0, y)
      case (Pos(x, 0), map) => map(Pos(x-1,0)) + value(x, 0)
      case (Pos(x, y), map) => map(Pos(x, y-1)) + map(Pos(x-1, y)) - map(Pos(x-1, y-1)) + value(x, y)
    })
  }


  def solvePart(k: Int, subSumArrays:TArray) = {
    def value(x:Int)(y:Int):Long =
      if(x < 0 || y < 0 || x >= 300 || y >= 300) 0 else subSumArrays(Pos(x,y))
    def calAt(x:Int, y:Int) =
      value(x+k-1)(y+k-1) - value(x-1)(y+k-1) - value(x+k-1)(y-1) + value(x-1)(y-1)

    Area(Pos(0, 0), Pos(300, 300)-Pos(k, k)).points
      .map{case pos@Pos(x, y) =>
        (pos+Pos(1,1), calAt(x, y))
      }.maxBy{_._2}

  }

  def solve(posValueMap:Input)  = solvePart(3, posValueMap)
  def solve2(posValueMap:Input) ={
    val ((pos, _), size) = {(1 to 300).par
      .map{ size => (solvePart(size, posValueMap), size) }
      .toList
      .maxBy{_._1._2}}
    (pos, size)
  }

}
