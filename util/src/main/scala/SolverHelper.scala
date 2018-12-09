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
      x <- (lt.x to rb.x).toIterator 
      y <- (lt.y to rb.y)
    } yield Pos(x, y)
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

