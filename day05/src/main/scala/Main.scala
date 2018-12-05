//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

import myutil._
import scala.collection.mutable._
import scala.annotation.tailrec

object Main extends Day(5) {
  type Input = String

  def processedInput = react(input.head.toList)

  @tailrec
  def react(charList:List[Char], processed:Stack[Char]=Stack(), isChanged:Boolean=false):String = {
    charList match {
      case Nil => if(isChanged) react(processed.toList.reverse) else processed.mkString("").reverse
      case a :: tail if processed.isEmpty => {
        processed.push(a)
        react(tail, processed, isChanged)
      }
      case a :: tail => {
        val popped = processed.pop
        if((popped - a).abs != 32) {
          processed.push(popped)
          processed.push(a)
        }
        react(tail, processed , isChanged)
      }
    }
  }
  def solve(input:Input) = input.size
  def solve2(input:Input) = {
    val keys = input.toUpperCase.toSet.toList
    keys.par.map{ key => 
      react(input.filterNot{c => c.toUpper == key}.toList)
    }.toList.map{_.length}.min
  }
}
