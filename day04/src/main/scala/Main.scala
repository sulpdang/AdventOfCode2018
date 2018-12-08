//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import scala.collection.mutable._
import scala.annotation.tailrec
import myutil._

object Main extends Day(4) {

  type MINUTES = Map[Int, List[Int]]
  type Input = MINUTES

  implicit class MyParser(val value:String) extends AnyVal {
    def minute:Int = value.substring(15,17).toInt
    def hour:Int   = value.substring(12,14).toInt
  }

  def processedInput = {
    @tailrec
    def travel(input:List[String], minutes:MINUTES = Map().withDefaultValue(List()),
               id:Int= -1, start:Int=0):MINUTES = {
      input match {
        case Nil => minutes
        case a::tail => {
          a(19) match {
            case 'G' => travel(tail, minutes, a.drop(26).takeInt)
            case 'f' => travel(tail, minutes, id, a.minute)
            case 'w' => 
              val end = a.minute
              minutes(id) ++= (start until end)
              travel(tail, minutes, id, end)
          }
        }
      }
    }
    travel(input.sorted)
  }

  def solve(input:Input) = {
    val (id, sleepMins) = input.maxBy{case (k, v) => v.size}
    val maxMinutes = sleepMins.groupBy{x=>x}.maxBy{case (k, v) => v.size}._1
    id * maxMinutes
  }
  def solve2(input:Input) = {
    val (id, min, _) = input.map{case (k, v) => 
      val (min, count) = v.groupBy(x=>x).mapValues(_.size).maxBy(_._2)
      (k, min, count)
    }.toList.maxBy{_._3}
    id*min
  }
}
