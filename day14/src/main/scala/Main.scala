//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._
import scala.collection.mutable._

object Main extends Day(14) {
  type Input = Int

  lazy val genIter = Iterator.iterate((ArrayBuffer(3, 7), List(3, 7), 0, 1)) { case (buff, _,  aLoc, bLoc) => {
      val newValue = buff(aLoc) + buff(bLoc)
      val nextList = if(newValue >= 10) List(1, newValue%10) else List(newValue%10)
      buff ++= nextList
      val length = buff.length
      (buff, nextList, (aLoc + buff(aLoc) + 1) % length, (bLoc + buff(bLoc) + 1) % length)
    }}

  def processedInput = input.head.toInt

  def solve(input:Input) = {
    genIter.map{_._2}.flatten.drop(input).take(10).mkString("")
  }
  def solve2(input:Input) = {
    val inputStr = input.toString
    (genIter.map{_._1}.dropWhile{ x=> 
      !(x.takeRight(10).mkString("") contains inputStr)
    }.next.length) - inputStr.length - 1
  }
}
