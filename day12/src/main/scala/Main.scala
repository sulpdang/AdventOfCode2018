//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._
import scala.collection._

object Main extends Day(12) {

  type Pots  = List[Int]
  type Note  = Array[Int]
  type Input = Iterator[State]

  implicit class IntUtil(val i:Int) extends AnyVal {
    def shift(e:Int) = ((i << 1) + e) & 31
  }

  case class State(state:Pots, startIndex:Int) {
    def shifted = State(state, startIndex-2)
    def trimmed = {
      def trimAcc(remain:Pots, newStartIndex:Int=startIndex):State = {
        remain match {
          case 0 :: tail  => trimAcc(tail, newStartIndex+1)
          case first :+ 0 => trimAcc(first, newStartIndex)
          case x          => State(x, newStartIndex)
        }
      }
      trimAcc(state)
    }

    def gen(note:Note) = {
      val nextState = (state.toIterator ++ Iterator.continually(0))
        .scanLeft(0){ _.shift(_) }
        .drop(1)
        .take(state.length + 3)
        .map{note}.toList
      State(nextState, startIndex)
    }

    lazy val sum = state.zipWithIndex.filter{_._1 == 1}.map{_._2 + startIndex}.sum
    def next(note:Note) = this.shifted.gen(note).trimmed
  }

  def processedInput = {

    val r = """([.#]{5}) => ([.#])""".r
    def toBinary(c:Char) = if(c == '.') 0 else 1
    val state = input.head.drop(15).map{toBinary(_)}
    val note = new Array[Int](32)
    input.drop(2).foreach{ case r(key, value) => 
      val keyToInt = Integer.parseInt(key.map{toBinary(_)}.mkString(""), 2)
      note(keyToInt) = toBinary(value.head)
    }
    Iterator.iterate(State(state.toList, 0)){_.next(note)}
  }

  def solve(input:Input) = input.drop(20).next.sum
  def solve2(input:Input) = {
    val (Seq(prev,next), i) = input
      .sliding(2)
      .zipWithIndex
      .dropWhile{case (Seq(a, b), _) => a.state != b.state}
      .next
    (next.sum - prev.sum) * (50000000000L - (i+1)) + next.sum
  }

}