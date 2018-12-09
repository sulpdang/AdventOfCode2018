//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import scala.collection.mutable.Stack
import myutil._

object Main extends Day(8) {
  type Input = List[Int]
  def processedInput = input.head.split(' ').map{_.toInt}.toList
  def makingTree(input:List[Int])(sumNode:(List[Int], List[Int]) => Int):Int = {
    def makingTreeAcc(remainInput:List[Int]):(List[Int], Int) = {
      remainInput match {
        case 0 :: b :: tail => (tail.drop(b), tail.take(b).sum)
        case a :: b :: tail => 
          val (nRemain, childValue) = Iterator.iterate((tail, List[Int]())){
            case (rem, res) => makingTreeAcc(rem).copy(_2 = res :+ _2)
          }.drop(a).next
          (nRemain.drop(b), sumNode(nRemain.take(b), childValue))
      }
    }
    makingTreeAcc(input)._2
  }
  
  def solve(input:Input) = {
    makingTree(input) { case (meta, child) => meta.sum + child.sum }
  }

  def solve2(input:Input) = {
    makingTree(input) { case (meta, child) => 
      meta.filter{_ <= child.size}.map{x=> child(x - 1)}.sum
    }
  }
}
