//
// Util
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//
//

package object myutil {
  import math._
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
      if(this.x == that.y) this.y - that.y
      else this.x - that.y
    }
  }


  object Util {

    def readContent:List[String] = {
      scala.io.Source.fromResource("input.txt")
        .mkString
        .split('\n').toList
    }

    def input = readContent

  }

  object SimpleHtml {

    import java.net._
    import util._

    def getInputFromSite(urlStr:String, session:String) = {
      val url = new URL(urlStr)
      val con = url.openConnection().asInstanceOf[HttpURLConnection]
      con.setRequestMethod("GET")
      con.setRequestProperty("Cookie", s"session=$session")
      con.setUseCaches(false)
      con.connect

      con.getResponseCode match {
        case 200 =>
          Success(scala.io.Source.fromInputStream(con.getInputStream).getLines.toList)
        case x   => Failure(new Exception(s"Not appropriate response code $x"))
      }
    }

  }

  abstract class Day(day:Int) extends App {

    type Input

    import scala.util._

    def sessionFile:String = "../util/.session"
    val inputFileName = "input.txt"

    def input = readInput.get

    def readContent:List[String] = getResource(inputFileName).map(_.split('\n').toList).get
    val readSession = Try(scala.io.Source.fromFile(sessionFile).mkString("").trim)
    def getResource(filename:String) =
      Try{
        val buf = scala.io.Source.fromFile(s"src/main/resources/$filename")
        val res = buf.mkString("")
        buf.close()
        res
      }

    def downloadInputContent(session:String):Try[List[String]] =
      SimpleHtml.getInputFromSite(
        s"https://adventofcode.com/2018/day/$day/input", session
      )

    def writeResource(fileName:String, content:String):Try[Unit] = {
      Try{
        import java.io._
        val file = new File(s"src/main/resources/$fileName")
        println(file.getAbsolutePath)
        val pw = new PrintWriter(file)
        pw.write(content)
        pw.close()
      }
    }

    def readInput:Try[List[String]] = {
      import java.net.URL
      getResource(inputFileName).map{_.split('\n').toList} recoverWith {
        case e:Throwable => {
          val content = readSession.flatMap{ downloadInputContent(_) }
          content.foreach{ x=> writeResource(inputFileName, x.mkString("\n")) }
          content
        }
      }

    }

    def printRes {

      println(s"--- Day $day ---")

      def num(e:Throwable) = {
        e.getStackTrace().toArray
          .find{_.getFileName contains "Main.scala" }.map{_.getLineNumber} getOrElse (-1)
      }

      def printSolution(func: => Any, prob:String) = {
        Try{func} match {
          case Success(x) => println(s"$prob: ${Option(x).getOrElse("null")}")
          case Failure(e) => println(s"$prob: Failed[line ${num(e)}](${e.getMessage()} , $e)")
        }
      }

      printSolution(solve{ processedInput }, "A")
      printSolution(solve2{ processedInput }, "B")

    }

    def processedInput:Input

    def solve(input:Input):Any
    def solve2(input:Input):Any

    printRes
  }
}
