//
// SolverHelper
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//


package object solver {
  import myutil._
  implicit class ValString(val str:String) extends AnyVal {
    def takeInt = str.takeWhile(_.isDigit).toInt
  }

  case class Area(val lt:Pos, val rb:Pos) {
    def points = for{
      x <- (lt.x to rb.x)
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
    def times[T](first:T)(t: (T,Int) => T):T = {
      (1 to i).foldLeft(first){t}
    }
  }
}

