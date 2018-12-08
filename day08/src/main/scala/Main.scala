//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import scala.collection.mutable.Stack
import myutil._

trait Tree {
  val meta:List[Int]
  val sum:Int
}

case class Node(meta:List[Int], child:List[Tree], 
  sumFunc:(List[Int], List[Tree]) => Int) extends Tree {
  val sum:Int = sumFunc(meta, child)
}
case class Leaf(val meta:List[Int]) extends Tree {
  val sum:Int = meta.sum
}

object Main extends Day(8) {
  type Input = List[Int]
  def processedInput = {
    input.head.split(' ').map{_.toInt}.toList
  }
  def makingTree(input:List[Int])(sumNode:(List[Int], List[Tree]) => Int):Tree = {
    def makingTreeAcc(remainInput:List[Int]):(List[Int], Tree) = {
      remainInput match {
        case a :: b :: tail if a == 0 => (tail.drop(b), Leaf(tail.take(b)))
        case a :: b :: tail => 
          val (nRemain, children) = (1 to a).foldLeft((tail, List[Tree]())){
            case ((rem, res), _) => 
              val (nextRem, tree) = makingTreeAcc(rem)
              (nextRem, res :+ tree) }
          (nRemain.drop(b), Node(nRemain.take(b), children, sumNode))
      }
    }
    makingTreeAcc(input)._2
  }
  def solve(input:Input) = {
    makingTree(input){case (meta, child) => meta.sum + child.map{_.sum}.sum }.sum
  }
  def solve2(input:Input) = {
    makingTree(input){case (meta, child) => 
      val childValues = child.map{_.sum}
      meta.map{ m => 
        if(m <= child.size) Some(childValues(m-1)) else None
      }.flatten.sum
    }.sum
  }
}
