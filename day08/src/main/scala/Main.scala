//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import scala.collection.mutable.Stack
import myutil._

trait Info
case object Child extends Info
case object ChildValue extends Info
case object Sum extends Info
case object Meta extends Info

object Main extends Day(8) {
  type Input = List[Int]
  def processedInput = {
    input.head.split(' ').map{_.toInt}.toList
  }
  def insertInfo(stack:Stack[Info], numChild:Int, numMeta:Int) = {
    stack.push(Sum)
    if(numChild > 0) {
      if(numMeta > 0) (1 to numMeta).foreach{i=>stack.push(ChildValue)}
      (1 to numChild).foreach{i=>stack.push(Child)}
    } else {
      if(numMeta > 0) (1 to numMeta).foreach{i=>stack.push(Meta)}
    }
    stack
  }
  def solve(input:Input) = {
    def solveAcc(ints:List[Int], stack:Stack[Info]=Stack(),res:Int = 0,
      stackChild:Stack[List[Int]]=Stack(), childValues:List[Int]=List()):Int = {
      ints match {
        case Nil => res
        case a::tail if stack.isEmpty =>
          solveAcc(tail.tail, insertInfo(stack, a, tail.head), res)
        case a::tail => 
          stack.pop match {
            case Child => solveAcc(tail.tail, insertInfo(stack, a, tail.head), res)
            case Meta | ChildValue => solveAcc(tail, stack, res + a)
            case Sum => solveAcc(ints, stack, res)
          }
      }
    }
    solveAcc(input)
  }
  def solve2(input:Input) = {
    def solve2Acc(ints:List[Int], stack:Stack[Info]=Stack(),res:Int = 0,
      stackChild:Stack[List[Int]]=Stack(), childValues:List[Int]=List()):Int= {
      ints match {
        case Nil => res
        case a::tail if stack.isEmpty =>
          solve2Acc(tail.tail, insertInfo(stack, a, tail.head))
        case a::tail => 
          stack.pop match {
            case Child => 
              stackChild.push(childValues)
              solve2Acc(tail.tail, insertInfo(stack, a, tail.head), stackChild= stackChild)
            case Meta => 
              solve2Acc(tail, stack, res+a, stackChild, childValues)
            case Sum => 
              val nextChildValue = stackChild.pop
              solve2Acc(ints, stack, 0, stackChild, nextChildValue :+ res)
            case ChildValue if a-1 >= childValues.size => 
              solve2Acc(tail, stack, res, stackChild, childValues)
            case ChildValue => 
              solve2Acc(tail, stack, res + childValues(a-1), stackChild, childValues)
          }
      }
    }
    solve2Acc(input)
  }
}
