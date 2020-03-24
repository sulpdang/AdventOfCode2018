//
// Main
//
// Copyright (c) 2019 Yohho (sulpdang@gmail.com)
//

package solver

import scala.util.{Try, Success, Failure}
import myutil._

object Main extends Day(19) {
  import Instruction._
  type Input = Seq[Assembly]

  var head:Int = _

  def processedInput = {
    import scala.util._
    head = input.head.drop(4).toInt
    inputToAssembly(input.tail)
  }
  def solve(input:Input) = {
    instructExecuteStream(input, head, Array.fill(6)(0))
    .dropWhile(reg => reg(head) < input.size).head(0)
  }
  def solve2(input:Input) = (1 to 10551376).filter{10551376 % _ == 0}.sum
}
