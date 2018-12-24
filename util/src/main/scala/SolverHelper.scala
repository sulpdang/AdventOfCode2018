//
// SolverHelper
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//


package object solver {
  import myutil._
  import math._
  implicit class ValString(val str:String) extends AnyVal {
    def takeInt = str.takeWhile(_.isDigit).toInt
  }

  case class Area(val lt:Pos, val rb:Pos) {
    def points = for{
      x <- (lt.x to rb.x).toIterator
      y <- (lt.y to rb.y)
    } yield Pos(x, y)

    def extend(amount:Int) = Area(lt - Pos(amount, amount), rb + Pos(amount, amount))
  }
  object Area {
    def minMaxArea(points:List[Pos]) = {
      def getMinMax(func:Pos=>Int) = 
        (func(points.minBy(func)),func(points.maxBy(func)))
      lazy val (minX, maxX) = getMinMax(_.x)
      lazy val (minY, maxY) = getMinMax(_.y)
      Area(Pos(minX, minY), Pos(maxX, maxY))
    }
  }
  implicit class IntUtil(val i:Int) extends AnyVal {
    def times(t: => Unit) {
      (1 to i).foreach{i=>t}
    }
    def foldLeft[T](first:T)(t: (T,Int) => T):T = {
      (1 to i).foldLeft(first){t}
    }
  }
  case class Pos(val x:Int, val y:Int) extends Ordered[Pos] {
    def this(arr:Array[String]) = this(arr(0).toInt, arr(1).toInt)
    def +(other:Pos) = Pos(x + other.x, y + other.y)
    def -(other:Pos) = Pos(x - other.x, y - other.y)
    def *(value:Int) = Pos(x * value, y * value)
    def /(value:Int) = Pos(x / value, y / value)
    def manhattan(that:Pos) = (this.x - that.x).abs + (this.y - that.y).abs
    def dist(that:Pos) = {
      val distX = this.x - that.x
      val distY = this.y - that.y
      sqrt(distX*distX + distY*distY)
    }
    def compare(that:Pos) = {
      if(this.y != that.y) this.y - that.y
      else this.x - that.x
    }
  }

}

