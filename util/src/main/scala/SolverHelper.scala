//
// SolverHelper
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//


package object solver {
  implicit class ValString(val str:String) extends AnyVal {
    def takeInt = str.takeWhile(_.isDigit).toInt
  }
}

