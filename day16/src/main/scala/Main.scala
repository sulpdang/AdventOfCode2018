//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import collection.mutable.{Map => MM}
import myutil._
object Main extends Day(16) {

  import Instruction._

  type Input = (Iterator[List[Array[Int]]], Iterator[Array[Int]])

  def processedInput = {
    val (learnRaw, codes) = input.map{_.trim}.filter{_.length > 0}.grouped(3)
      .partition{
        case head::tail => head.startsWith("Before")
        case Nil => false
      }
    val learn = learnRaw
      .map{case List(a,b,c) =>
        val before   = a.drop(9).dropRight(1).split(",").map{_.trim.toInt}.toArray
        val instruct = b.split(" ").map{_.trim.toInt}.toArray
        val after    = c.drop(9).dropRight(1).split(",").map{_.trim.toInt}.toArray
        List(before, instruct, after)
      }
    (learn, codes.flatten.map{_.split(' ').map{_.toInt}})
  }

  lazy val allInst = List( Addr, Addi, Mulr,
          Muli, Banr, Bani, Borr, Bori, Setr, Seti,
          Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr)

  def instFilter(before:Register, after:Register, a:Int, b:Int, c:Int)(inst:Instruct) = {
    val clone = before.clone
    inst.execute(a, b, c, clone)
    clone.sameElements(after)
  }

  def solve(input:Input) = {
    input._1.map { case List(before, Array(_, a, b, c), after) =>
        allInst.filter{instFilter(before, after, a, b, c)}
    }.filter{_.size >= 3}.size
  }

  def solve2(input:Input) = {
    val (learn, codes) = input
    val target = learn.foldLeft(MM[Int, InstructSet]().withDefaultValue(allInst.toSet)) {
      case (instMap, List(before, Array(op, a, b, c), after)) =>
        instMap(op) = instMap(op).filter{instFilter(before, after, a, b, c)}
        instMap
    }
    def refineInstruct(remain:List[(Int,InstructSet)], 
      processed:List[(Int, InstructSet)]=List(), known:InstructSet =Set()):Array[Instruct] = {
      remain match {
        case Nil => processed.sortBy{_._1}.map{_._2.head}.toArray
        case x => {
          val (oneSize, other) = remain.partition{_._2.size == 1}
          val newKnown = known ++ oneSize.map{_._2.head}
          val newRemain = other.map{case (op, insts) => (op, insts.diff(newKnown)) }
          refineInstruct(newRemain, processed ++ oneSize, newKnown)
        }
      }
    }

    val instSet = refineInstruct(target.toList.sortBy{_._2.size})
    codes.foldLeft(Array(0,0,0,0)){case (reg, Array(op, a, b, c))=>
      instSet(op).execute(a, b, c, reg)
      reg
    }.apply(0)
  }
}
