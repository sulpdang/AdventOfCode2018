//
// solver.Main
//
// Copyright (c) 2019 Yohho (sulpdang@gmail.com)
//
package solver

import myutil._
import language.implicitConversions
import scala.collection.immutable.SortedSet

object Main extends Day(23) {

  type Input     = List[NanoBot]
  type Size      = Int
  type Big       = BigDecimal
  type Manhattan = Big

  case class NanoBot(pos3d:Pos3D, range:Big, id:Int) {
    def inRange(pos:Pos3D):Boolean = (this.pos3d |-| pos) <= range
  }

  case class InEquality(a:Big, b:Big) {
    require(a <= b)
    def split = {
      if(a == b) List(this)
      else {
        def floor = BigDecimal.RoundingMode.FLOOR
        val range = b - a 
        val a1 = (a + range / 2.0).setScale(0, floor)
        List(InEquality(a, a1), InEquality(a1+1, b))
      }
    }
  }

  case class Box(min:Pos3D, max:Pos3D) {
    require(min.x <= max.x && min.y <= max.y && min.z <= max.z, s"${min} ${max}")
    def isDot = (min.x == max.x) && min.y == max.y && min.z == max.z
    private def closeAxis(pos:Pos3D)(f:Pos3D => Big) = {
      val value = f(pos)
      if(value < f(min)) f(min) else
      if(value > f(max)) f(max) else value
    }
    private def closestPos(pos:Pos3D) = Pos3D(closeAxis(pos)(_.x),
      closeAxis(pos)(_.y), closeAxis(pos)(_.z))
    def isOverlapped(bot:NanoBot) = bot.inRange(closestPos(bot.pos3d))
    def split = {
      if(isDot) Nil else {
        for {
          x <- InEquality(min.x, max.x).split
          y <- InEquality(min.y, max.y).split
          z <- InEquality(min.z, max.z).split
        } yield Box(Pos3D(x.a, y.a, z.a), Pos3D(x.b, y.b, z.b))
      }
    }
  }

  def processedInput = {
    implicit def strToBigDec(value:String):Big = BigDecimal(value)
    lazy val parsePattern = "pos=<(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)>, r=([0-9]+)".r.unanchored

    input.zipWithIndex.map { case (parsePattern(x,y,z, range), i) =>
      NanoBot(Pos3D(x, y, z), range, i)}
  }

  def solve(input:Input) = {
    val maxNanobot = input.maxBy{_.range}
    input.count{nano => maxNanobot.inRange(nano.pos3d)}
  }

  def solve2(input:Input) = {
    import collection.mutable._
    implicit def compare:Ordering[Pos3D] = Ordering.by{case a => -a.manhattanOrigin}
    val initBox = {
      def getPos(s:List[BigDecimal] => BigDecimal)(f:Pos3D => BigDecimal) = s(input.map{_.pos3d}.map{f})
      def minPos = getPos(_.min - 1) _
      def maxPos = getPos(_.max + 1) _
      Box(
        Pos3D(minPos(_.x), minPos(_.y), minPos(_.z)),
        Pos3D(maxPos(_.x), maxPos(_.y), maxPos(_.z))
      )
    }
    val queue:PriorityQueue[(Box, Int)] = PriorityQueue.empty(Ordering.by{case (box, upper) => 
      (upper, box.min)})
    queue.enqueue((initBox, input.size))
    Iterator.iterate((initBox, input.size)){case (smallBox, size) => 
      smallBox.split.foreach{box => queue.enqueue((box, input.count{box.isOverlapped}))}
      queue.dequeue
    }.dropWhile(!_._1.isDot).next()._1.min.manhattanOrigin
  }
}